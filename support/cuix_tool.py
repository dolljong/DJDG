#!/usr/bin/env python3
"""
cuix_tool.py - CUIX 파일 압축 해제/재압축 유틸리티

CUIX 파일은 ZIP 형식의 AutoCAD 커스터마이제이션 패키지입니다.
이 도구를 사용하면 CUI(XML) 파일을 텍스트 에디터로 직접 수정할 수 있습니다.

사용법:
    python cuix_tool.py <cuix파일명> extract    CUIX를 같은 이름의 폴더로 압축 해제
    python cuix_tool.py <cuix파일명> pack       같은 이름의 폴더를 CUIX로 재압축

예시:
    python cuix_tool.py djdg_new.cuix extract
    # -> djdg_new/ 폴더에 압축 해제
    # ... CUI 파일을 편집기로 수정 ...
    python cuix_tool.py djdg_new.cuix pack
    # -> djdg_new/ 폴더를 djdg_new.cuix로 재압축
"""

import os
import sys
import zipfile
import xml.etree.ElementTree as ET


def _get_folder(cuix_path):
    """CUIX 파일명에서 폴더 경로를 결정합니다. (확장자 제거)"""
    return os.path.splitext(cuix_path)[0]


def extract(cuix_path):
    """CUIX(ZIP) 파일을 같은 이름의 폴더로 압축 해제합니다."""
    if not os.path.exists(cuix_path):
        print(f"오류: 파일을 찾을 수 없습니다: {cuix_path}")
        return False

    output_dir = _get_folder(cuix_path)

    print(f"압축 해제: {cuix_path}")
    print(f"출력 폴더: {output_dir}")

    with zipfile.ZipFile(cuix_path, 'r') as zf:
        zf.extractall(output_dir)
        names = zf.namelist()

    cui_count = sum(1 for n in names if n.endswith('.cui'))
    xml_count = sum(1 for n in names if n.endswith('.xml'))
    bmp_count = sum(1 for n in names if n.lower().endswith('.bmp'))

    print(f"\n추출 완료:")
    print(f"  CUI 파일: {cui_count}개")
    print(f"  XML 파일: {xml_count}개")
    print(f"  BMP 아이콘: {bmp_count}개")
    print(f"  총 파일: {len(names)}개")
    print(f"\n이제 {output_dir}/ 폴더의 CUI 파일을 편집하세요.")
    print(f"편집 후: python cuix_tool.py {os.path.basename(cuix_path)} pack")
    return True


def pack(cuix_path):
    """같은 이름의 폴더를 CUIX(ZIP)로 재압축합니다."""
    input_dir = _get_folder(cuix_path)

    if not os.path.isdir(input_dir):
        print(f"오류: 폴더를 찾을 수 없습니다: {input_dir}")
        return False

    # XML 유효성 검사
    print(f"XML 유효성 검사 중...")
    errors = []
    for root_dir, dirs, filenames in os.walk(input_dir):
        for fn in filenames:
            if fn.endswith('.cui') or fn.endswith('.xml'):
                fpath = os.path.join(root_dir, fn)
                try:
                    with open(fpath, 'r', encoding='utf-8-sig') as f:
                        content = f.read()
                    ET.fromstring(content)
                except ET.ParseError as e:
                    rel = os.path.relpath(fpath, input_dir)
                    errors.append((rel, str(e)))
                except Exception as e:
                    rel = os.path.relpath(fpath, input_dir)
                    errors.append((rel, str(e)))

    if errors:
        print(f"\nXML 오류 {len(errors)}개 발견:")
        for name, err in errors:
            print(f"  {name}: {err}")
        print("\n오류를 수정한 후 다시 시도하세요.")
        return False

    print("  XML 유효성 검사 통과")

    # 파일 수집 (상대 경로 기준)
    all_files = []
    for root_dir, dirs, filenames in os.walk(input_dir):
        for fn in filenames:
            fpath = os.path.join(root_dir, fn)
            rel = os.path.relpath(fpath, input_dir).replace('\\', '/')
            all_files.append((rel, fpath))

    # 패키징 순서: 메타데이터 먼저, CUI, 나머지
    meta_files = ['[Content_Types].xml', '_rels/.rels', 'Menu_Package_Info.xml']
    ordered = []

    # 1) 메타데이터
    for meta in meta_files:
        for rel, fpath in all_files:
            if rel == meta:
                ordered.append((rel, fpath))
                break

    # 2) CUI 파일
    for rel, fpath in all_files:
        if rel.endswith('.cui') and (rel, fpath) not in ordered:
            ordered.append((rel, fpath))

    # 3) 나머지 (BMP 등)
    ordered_set = set(r for r, _ in ordered)
    for rel, fpath in all_files:
        if rel not in ordered_set:
            ordered.append((rel, fpath))

    # 백업
    if os.path.exists(cuix_path):
        bak = cuix_path + '.bak'
        if os.path.exists(bak):
            os.remove(bak)
        os.rename(cuix_path, bak)
        print(f"기존 파일 백업: {bak}")

    # ZIP 생성
    print(f"\nCUIX 생성: {cuix_path}")
    with zipfile.ZipFile(cuix_path, 'w', zipfile.ZIP_DEFLATED) as zf:
        for rel, fpath in ordered:
            zf.write(fpath, rel)

    cui_count = sum(1 for r, _ in ordered if r.endswith('.cui'))
    bmp_count = sum(1 for r, _ in ordered if r.lower().endswith('.bmp'))

    cuix_size = os.path.getsize(cuix_path)
    print(f"\n패키징 완료:")
    print(f"  CUI 파일: {cui_count}개")
    print(f"  BMP 아이콘: {bmp_count}개")
    print(f"  총 파일: {len(ordered)}개")
    print(f"  CUIX 크기: {cuix_size:,} bytes")

    # MNR 캐시 삭제
    cuix_basename = os.path.splitext(os.path.basename(cuix_path))[0]
    cuix_dir = os.path.dirname(cuix_path) or '.'
    for suffix in ['', '_light']:
        mnr = os.path.join(cuix_dir, f'{cuix_basename}{suffix}.mnr')
        if os.path.exists(mnr):
            os.remove(mnr)
            print(f"  MNR 캐시 삭제: {os.path.basename(mnr)}")

    print(f"\nAutoCAD에서 CUIX를 다시 로드하세요.")
    return True


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        return

    cuix_path = sys.argv[1]
    cmd = sys.argv[2].lower()

    if cmd == 'extract':
        extract(cuix_path)
    elif cmd == 'pack':
        pack(cuix_path)
    else:
        print(f"알 수 없는 명령: {cmd}")
        print("사용법: python cuix_tool.py <cuix파일명> [extract|pack]")


if __name__ == '__main__':
    main()
