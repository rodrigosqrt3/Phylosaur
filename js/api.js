// ═══════════════════════════════════════════════
// WIKIPEDIA & WIKIMEDIA API
// ═══════════════════════════════════════════════
async function fetchWikipediaInfo(cladeName) {
  try {
    const searchRes = await fetch(
      `https://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=${encodeURIComponent(cladeName)}&format=json&origin=*`
    );
    const searchData = await searchRes.json();
    
    if (!searchData.query.search.length) return null;

    const pageTitle = searchData.query.search[0].title;
    const pageUrl = `https://en.wikipedia.org/wiki/${encodeURIComponent(pageTitle.replace(/ /g, '_'))}`;

    const extractRes = await fetch(
      `https://en.wikipedia.org/w/api.php?action=query&titles=${encodeURIComponent(pageTitle)}&prop=pageimages|extracts&format=json&pithumbsize=300&exintro=1&explaintext=1&origin=*`
    );
    const extractData = await extractRes.json();
    const pages = extractData.query.pages;
    const page = pages[Object.keys(pages)[0]];

    return {
      title: pageTitle,
      url: pageUrl,
      image: page.thumbnail?.source || null,
      description: page.extract || null
    };
  } catch (e) {
    console.error('Wiki info error:', e);
    return null;
  }
}

async function fetchWikimediaImage(taxonName) {
  try {
    const fileName = `${taxonName} TD.png`;
    const url = `https://commons.wikimedia.org/w/api.php?action=query&titles=File:${encodeURIComponent(fileName)}&prop=imageinfo&iiprop=url&format=json&origin=*`;
    const res = await fetch(url);
    const data = await res.json();
    const pages = data.query.pages;
    const page = pages[Object.keys(pages)[0]];
    if (page['-1']) return null;
    return page?.imageinfo?.[0]?.url || null;
  } catch (e) {
    console.error('Wikimedia image error:', e);
    return null;
  }
}