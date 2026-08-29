# Personal Website

Source code for my personal academic website: https://www.trevorincerti.com.

The site is a custom static HTML/CSS site hosted on GitHub Pages. Jekyll is disabled via `.nojekyll`, so files are served directly from this repository and changes pushed to `master` go live within a few minutes.

## Site structure

| File/Folder | Description |
|---|---|
| `index.html` | Landing page with bio, photo, and hero banner |
| `working-papers.html` | Searchable, sortable working papers table |
| `publications.html` | Searchable, sortable publications table |
| `data.html` | Datasets and replication materials |
| `teaching.html` | Course syllabi and lesson materials |
| `collaboration.html` | Interactive coauthor network and 3D invited presentations globe |
| `contact.html` | Contact information |
| `japanese.html` | Japanese-language version of the landing page |
| `css/style.css` | Main stylesheet (Inter font, white background, light theme) |
| `js/table.js` | Logic for searchable and sortable paper tables |
| `files/` | PDFs, interactive HTML visualizations, and other linked assets |
| `images/` | Photos and images used across the site |
| `teaching/` | Interactive course syllabi and lesson HTML files |

## Dependencies

No build tools or package managers are required to edit the site. The only external dependencies are loaded via CDN at runtime:
- [Inter](https://fonts.google.com/specimen/Inter) — body font (Google Fonts)
- [globe.gl](https://globe.gl) — WebGL 3D globe on the Collaboration page
