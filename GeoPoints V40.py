#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os, math, re, json, urllib.request, time
from pathlib import Path

def get_elev(lat, lon):
    for ds in ['mapzen', 'aster30m', 'srtm30m']:
        try:
            url = f"https://api.opentopodata.org/v1/{ds}?locations={lat},{lon}"
            req = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
            with urllib.request.urlopen(req, timeout=5) as r:
                data = json.loads(r.read().decode())
            if data.get('status') == 'OK' and data.get('results'):
                e = data['results'][0].get('elevation')
                if e and -500 < e < 9000:
                    return e, ds
            time.sleep(0.05)
        except:
            continue
    return None, None

def interpolate(known, target):
    if not known: return None
    tx, ty = target
    dists = [(math.sqrt((x-tx)**2 + (y-ty)**2), z) for x, y, z in known]
    dists.sort()
    neighbors = dists[:5]
    w_sum, t_w = 0, 0
    for d, z in neighbors:
        if d < 0.1: return z
        w = 1.0 / (d ** 2)
        w_sum += w * z
        t_w += w
    return w_sum / t_w if t_w > 0 else None

def parse_coords(coords_str):
    # Формат: 51°05'56.92"N 36°32'32.40"E
    pattern = r'(\d+)°(\d+)\'([\d.]+)"([NS])\s+(\d+)°(\d+)\'([\d.]+)"([EW])'
    match = re.search(pattern, coords_str)
    if match:
        lat_d, lat_m, lat_s, lat_dir = match.group(1, 2, 3, 4)
        lon_d, lon_m, lon_s, lon_dir = match.group(5, 6, 7, 8)
        lat = float(lat_d) + float(lat_m)/60 + float(lat_s)/3600
        lon = float(lon_d) + float(lon_m)/60 + float(lon_s)/3600
        if lat_dir == 'S': lat = -lat
        if lon_dir == 'W': lon = -lon
        return lat, lon
    # Формат: 51.099144, 36.542333
    pattern2 = r'([-\d.]+)[,\s]+([-\d.]+)'
    match2 = re.search(pattern2, coords_str)
    if match2:
        return float(match2.group(1)), float(match2.group(2))
    return None, None

def to_wgs(x, y, rlat, rlon, rx, ry):
    m_lat = 111132.92
    m_lon = 111412.84 * math.cos(math.radians(rlat))
    return rlat + (y-ry)/m_lat, rlon + (x-rx)/m_lon

d = str(Path.home() / "Desktop")
inp = os.path.join(d, "geopoints_import.txt")
out = os.path.join(d, "geopoints_ready.txt")

print("="*80)
print("UNIVERSAL ELEVATION GETTER")
print("="*80 + "\n")

coords = []
with open(inp, 'r') as f:
    for line in f:
        p = line.strip().split()
        if len(p) >= 2:
            coords.append((float(p[0]), float(p[1])))

print(f"Прочитано координат: {len(coords)}\n")
print("="*80)
print("ОПОРНАЯ ТОЧКА")
print("="*80)
print("\nНайдите ЛЮБОЙ объект (угол здания, столб) видимый в проекте и Google Earth\n")

msk = input("МСК зона: ").strip()
print("\nКоординаты ОПОРНОЙ точки в проекте (МСК):")
ref_x = float(input("  X = ").strip())
ref_y = float(input("  Y = ").strip())

print("\nКоординаты ЭТОЙ ЖЕ точки из Google Earth:")
print('(Формат: 51°05\'56.92"N 36°32\'32.40"E или 51.099144, 36.542333)')
coords_str = input("  Координаты: ").strip()
rlat, rlon = parse_coords(coords_str)

if not rlat:
    print("\n❌ Ошибка парсинга")
    input("Enter...")
    exit()

print(f"\n✓ Опорная точка:")
print(f"  МСК:   X={ref_x:.3f}, Y={ref_y:.3f}")
print(f"  WGS84: {rlat:.6f}°N, {rlon:.6f}°E\n")
print("="*80)
print("ПОЛУЧАЮ ВЫСОТЫ")
print("="*80 + "\n")

results, known = [], []

for i, (x, y) in enumerate(coords):
    lat, lon = to_wgs(x, y, rlat, rlon, ref_x, ref_y)
    print(f"[{i+1:3d}/{len(coords)}] {lat:.5f}, {lon:.5f}...", end=' ')
    e, ds = get_elev(lat, lon)
    if e:
        print(f"✓ {e:.1f}m ({ds})")
        results.append((x, y, e))
        known.append((x, y, e))
    else:
        print("—")
        results.append((x, y, None))

print(f"\n✓ API: {len(known)}/{len(coords)} ({100*len(known)//len(coords) if coords else 0}%)")

if known and len(known) < len(coords):
    print("\n" + "="*80)
    print("ИНТЕРПОЛЯЦИЯ")
    print("="*80 + "\n")
    interp = 0
    for i in range(len(results)):
        if results[i][2] is None:
            print(f"[{i+1:3d}/{len(coords)}]...", end=' ')
            e = interpolate(known, (results[i][0], results[i][1]))
            if e:
                print(f"✓ {e:.1f}m")
                results[i] = (results[i][0], results[i][1], e)
                interp += 1
            else:
                print("—")
    print(f"\n✓ Интерполировано: {interp}")

with open(out, 'w') as f:
    for x, y, z in results:
        f.write(f"{x:.3f} {y:.3f} {z if z else 0.0:.3f}\n")

ok = sum(1 for r in results if r[2] and r[2] > 0)
print("\n" + "="*80)
print("ИТОГО")
print("="*80)
print(f"✅ Успешно: {ok}/{len(coords)} ({100*ok//len(coords) if coords else 0}%)")
print(f"📁 {out}")
print("="*80)
input("\nEnter...")