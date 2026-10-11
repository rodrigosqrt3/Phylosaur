<p align="center">
  <a href="https://rodrigosqrt3.github.io/Phylosaur/">
    <img src="pwa-icon.svg" width="144" height="144" alt="Phylosaur golden dinosaur footprint">
  </a>
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
- **Private challenges** let friends play the same hidden genus through a shared
  code. When a round ends, a rematch can be started straight away.
- **The Museum** keeps every genus you have discovered, with an image, a short
  description and its place in a browsable Clade Atlas.
- **Five difficulty levels** arrange genera by public familiarity, using thirty-day English Wikipedia pageviews as a proxy.

Pageviews are not treated as a measure of scientific importance, fossil
completeness, or phylogenetic complexity. They are an imperfect but reproducible
way to distinguish familiar names from less widely recognized taxa.

There are also achievements, some for streaks and quick wins and others for
filling the Museum, from a quarter of all theropods to every known spinosaurid
or abelisaurid in the game. The home screen shows a curiosity of the day about
a different clade, and signed-in players get a statistics page with their best
streak, guesses per win and recent games.

Typing a guess is forgiving: accents and capitalisation are ignored, a full
binomial such as *Tyrannosaurus rex* is accepted, and small spelling mistakes
get a suggestion instead of an error. The tree can be zoomed and individual
clades can be collapsed when it gets crowded.

## Languages and devices

Phylosaur is available in English, Portuguese (Brazil), Spanish, French, German
and Japanese. The language follows the browser by default and can be changed at
any time from the menu. Clade and genus names stay in their scientific form in
every language.

The game runs in any modern browser, on phones as well as on desktop, and has a
light and a dark theme. It can be installed as an app from the browser menu,
and the interface still opens without a connection, although guesses need to
reach the server.

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

## License

Copyright © 2026 Rodrigo Fonseca Villa. All rights reserved. The source code is
publicly visible, but permission is not granted to copy, modify, redistribute,
rehost, or create derivative works. Third-party materials retain their original
licences. See [LICENSE](LICENSE) for the complete terms.