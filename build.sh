cp README.md ThinkingGrid/. || exit 1
sed -i '' 's|ThinkingGrid/man/figures/|man/figures/|g' ThinkingGrid/README.md
sed -i '' '/#### Development Version (GitHub)/,/```/d' ThinkingGrid/README.md
R CMD build ThinkingGrid
rm ThinkingGrid/README.md
