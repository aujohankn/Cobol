from pathlib import Path

data = Path("Transaktioner.txt").read_bytes()

for encoding in ["utf-8", "cp1252", "iso-8859-1", "cp850"]:
    try:
        text = data.decode(encoding)
        print(f"\nEncoding: {encoding}")
        print(repr(text.splitlines()[0][45:95]))
    except UnicodeDecodeError:
        print(f"{encoding}: Invalid encoding")