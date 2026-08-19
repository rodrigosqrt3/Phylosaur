// ═══════════════════════════════════════════════
// WIKIPEDIA & WIKIMEDIA API
// ═══════════════════════════════════════════════
function getAnalyticsVisitorId() {
  const storageKey = 'phylosaur-visitor-id';
  let visitorId = localStorage.getItem(storageKey);
  if (visitorId && /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(visitorId)) {
    return visitorId;
  }

  if (crypto.randomUUID) {
    visitorId = crypto.randomUUID();
  } else {
    visitorId = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, character => {
      const random = Math.floor(Math.random() * 16);
      const value = character === 'x' ? random : (random & 0x3) | 0x8;
      return value.toString(16);
    });
  }
  localStorage.setItem(storageKey, visitorId);
  return visitorId;
}

async function callGameApi(action, payload = {}) {
  const { data: { session } } = await sb.auth.getSession();
  const accessToken = session?.access_token || SUPABASE_ANON_KEY;

  const response = await fetch(GAME_API_URL, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'apikey': SUPABASE_ANON_KEY,
      'Authorization': `Bearer ${accessToken}`
    },
    body: JSON.stringify({ action, visitorId: getAnalyticsVisitorId(), ...payload })
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

async function initializeAnalyticsAccess() {
  analyticsAccessChecked = true;
  if (!currentUserId) {
    isAnalyticsAdmin = false;
    return false;
  }

  try {
    const data = await callGameApi('analytics_access');
    isAnalyticsAdmin = data.allowed === true;
  } catch (error) {
    isAnalyticsAdmin = false;
  }
  return isAnalyticsAdmin;
}

function getStoredGameSessionIds() {
  const sessionIds = [];

  for (let index = 0; index < localStorage.length; index++) {
    const key = localStorage.key(index);
    if (!key?.startsWith('phylosaur-session:')) continue;

    const sessionId = localStorage.getItem(key);
    if (sessionId && !sessionIds.includes(sessionId)) {
      sessionIds.push(sessionId);
    }
  }

  return sessionIds.slice(0, 10);
}

function getGameSessionStorageKey(mode, difficulty) {
  const date = new Date().toISOString().slice(0, 10);
  return `phylosaur-session:${mode}:${difficulty}:${mode === 'daily' ? date : 'current'}`;
}

function getChallengeSessionStorageKey(code) {
  const normalizedCode = String(code || '').toUpperCase().replace(/[^A-Z0-9]/g, '');
  return `phylosaur-session:challenge:${normalizedCode}`;
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
