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

// ═══════════════════════════════════════════════
// PHYLOSAUR GAME SERVER
// ═══════════════════════════════════════════════
const PHYLOSAUR_GAME_URL = `${SUPABASE_URL}/functions/v1/phylosaur-game`;

async function callPhylosaurGame(action, payload = {}) {
  const response = await fetch(PHYLOSAUR_GAME_URL, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'apikey': SUPABASE_ANON_KEY,
      'Authorization': `Bearer ${SUPABASE_ANON_KEY}`
    },
    body: JSON.stringify({ action, ...payload })
  });

  let data;
  try {
    data = await response.json();
  } catch (error) {
    throw new Error('The game server returned an invalid response.');
  }

  if (!response.ok || !data?.ok) {
    const error = new Error(data?.error || 'The game server request failed.');
    error.status = response.status;
    error.data = data;
    throw error;
  }

  return data;
}

function startPhylosaurSession(mode, difficulty) {
  return callPhylosaurGame('start', { mode, difficulty });
}

function loadPhylosaurSession(sessionId) {
  return callPhylosaurGame('state', { sessionId });
}

function submitPhylosaurGuess(sessionId, guess) {
  return callPhylosaurGame('guess', { sessionId, guess });
}

function requestPhylosaurHint(sessionId) {
  return callPhylosaurGame('hint', { sessionId });
}

function giveUpPhylosaurSession(sessionId) {
  return callPhylosaurGame('give_up', { sessionId });
}