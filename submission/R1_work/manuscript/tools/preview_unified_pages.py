"""Create page contact sheets for visual manuscript QA."""
from pathlib import Path
from PIL import Image, ImageDraw
from pypdf import PdfReader

ROOT = Path(__file__).resolve().parents[1]
for language in ("en", "zh"):
    folder = ROOT / f"qa_unified_figures_{language}"
    pages = sorted(folder.glob("page-*.png"), key=lambda p: int(p.stem.split("-")[-1]))
    for start in range(0, len(pages), 8):
        sheet = Image.new("RGB", (1600, 1100), "#d7d7d7")
        draw = ImageDraw.Draw(sheet)
        for index, path in enumerate(pages[start:start+8]):
            page = Image.open(path).convert("RGB")
            page.thumbnail((390, 515))
            x, y = (index % 4) * 400, (index // 4) * 550
            sheet.paste(page, (x + (400-page.width)//2, y+25))
            draw.text((x+10, y+5), f"Page {start+index+1}", fill="black")
        sheet.save(folder / f"contact-{start//8+1}.png")
    pdf = PdfReader(folder / f"manuscript_r1_{language}_unified_figures.pdf")
    print(language, "pages", len(pdf.pages))
