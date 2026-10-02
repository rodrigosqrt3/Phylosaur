<p align="center">
  <img src="pwa-icon.svg" width="144" height="144" alt="Phylosaur golden dinosaur footprint">
</p>

<h1 align="center">Phylosaur</h1>

<p align="center"><em>A phylogenetic classification challenge</em></p>

<p align="center">
  <a href="https://rodrigosqrt3.github.io/Phylosaur/"><strong>Play Phylosaur</strong></a>
  ·
  <a href="https://rodrigosqrt3.github.io/Phylosaur/about.html">Methodology and scientific background</a>
</p>

Phylosaur is an independent educational game about the dinosaur family tree.
The player identifies a hidden genus by making taxonomic guesses. After each
attempt, the game reveals the deepest named clade shared by the guess and the
target, gradually turning the classification into a navigable tree.

## Playing the game

- **Daily challenges** use the same target worldwide for each level and UTC date.
- **Practice mode** provides unlimited games without affecting Daily statistics.
- **Private challenges** let friends play the same hidden genus through a shared code.
- **The Museum** records discovered genera and organizes them in a browsable Clade Atlas.
- **Five difficulty levels** arrange genera by public familiarity, using thirty-day English Wikipedia pageviews as a proxy.

Pageviews are not treated as a measure of scientific importance, fossil
completeness, or phylogenetic complexity. They are an imperfect but reproducible
way to distinguish familiar names from less widely recognized taxa.

## Scientific approach

The catalogue is a curated working classification rather than a claim that
there is one final dinosaur phylogeny. Stored lineages combine automated data
collection with manual review and are checked for ambiguous names, synonyms,
placeholder ranks, inconsistent parent relationships, and incompatible lineage
depths.

Principal sources include:

- the [Paleobiology Database](https://paleobiodb.org/) for fossil occurrence and stratigraphic evidence;
- The Taxonomicon through `taxodist` for taxonomic lineage candidates;
- original descriptions, modern systematic revisions, and museum research pages for manual decisions;
- the [Wikimedia Pageviews API](https://wikimedia.org/api/rest_v1/) for difficulty calibration;
- reusable media from Wikimedia Commons and other explicitly licensed sources.

Phylogenetic placements change as new specimens and analyses are published.
Phylosaur therefore records conservative, game-compatible paths and documents
uncertain cases instead of presenting the tree as immutable. The application is
an educational resource and should not be cited as a primary scientific source.

## Development and feedback

Phylosaur is under active development. Reports of taxonomic inaccuracies,
interface problems, missing genera, and other suggestions are welcome at
[rodrigo03.villa@gmail.com](mailto:rodrigo03.villa@gmail.com).

The original game concept was inspired by [Metazooa](https://metazooa.com/).