"""Check exported fonts and assemble a compact figure-review PDF."""
from pathlib import Path
import subprocess
from pypdf import PdfReader, PdfWriter

ROOT = Path(__file__).resolve().parents[2]
NAMES = ["figure_1_estimand_geometry", "figure_2_joint_posterior",
         "figure_3_robustness", "figure_s2_posterior_predictive_checks"]
writer = PdfWriter()
for name in NAMES:
    path = ROOT / "figures" / (name + ".pdf")
    fonts = subprocess.check_output(["/opt/homebrew/bin/pdffonts", str(path)], text=True)
    lines = fonts.splitlines()[2:]
    assert lines and all("Optima" in line and "yes yes" in line for line in lines), fonts
    pdf = PdfReader(path)
    assert len(pdf.pages) == 1
    writer.add_page(pdf.pages[0])
    subprocess.run(["/opt/homebrew/bin/pdftoppm", "-singlefile", "-scale-to", "1600", "-png",
                    str(path), str(Path("/private/tmp") / (name + "_unified_qa"))], check=True)
    print(f"Embedded Optima fonts and one-page PDF verified: {name}")
writer.add_metadata({"/Author": "Miura Meng", "/Title": "Racial Imprisonment Disparity: Figures",
                     "/Producer": "", "/Creator": ""})
writer.write(ROOT / "figures" / "figures_unified_review.pdf")
