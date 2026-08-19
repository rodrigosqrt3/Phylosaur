// ═══════════════════════════════════════════════
// WIKIPEDIA & WIKIMEDIA API
// ═══════════════════════════════════════════════
async function callGameApi(action, payload = {}) {
  const response = await fetch(GAME_API_URL, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'apikey': SUPABASE_ANON_KEY,
      'Authorization': `Bearer ${SUPABASE_ANON_KEY}`
    },
    body: JSON.stringify({ action, ...payload })
  });

  let data = null;
  try {
    data = await response.json();
  } catch (error) {
    data = { ok: false, error: 'The game server returned an invalid response.' };
  }

  if (!response.ok || !data?.ok) {
    const apiError = new Error(data?.error || 'The game server is unavailable.');
    apiError.status = response.status;
    apiError.data = data;
    throw apiError;
  }

  return data;
}

function getGameSessionStorageKey(mode, difficulty) {
  const date = new Date().toISOString().slice(0, 10);
  return `phylosaur-session:${mode}:${difficulty}:${mode === 'daily' ? date : 'current'}`;
}

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
