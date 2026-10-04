// ═══════════════════════════════════════════════
// WIKIPEDIA & WIKIMEDIA API
// ═══════════════════════════════════════════════
const wikipediaInfoCache = new Map();
const wikimediaImageCache = new Map();
window.phylosaurPerformance = window.phylosaurPerformance || [];
const GAME_API_TIMEOUT_MS = 15000;

function getLocalizedGameApiError(status, serverMessage = '') {
  if (currentLocale === 'en' && serverMessage) return serverMessage;
  if (status === 401) return t('api.signInRequired');
  if (status === 403) return t('api.notAllowed');
  if (status === 404) return t('api.notFound');
  if (status === 409) return t('api.conflict');
  if (status === 410) return t('api.expired');
  if (status >= 400 && status < 500) return t('api.invalidRequest');
  return t('api.unavailable');
}

function recordGameApiPerformance(action, durationMs, ok) {
  const entry = {
    action,
    durationMs: Math.round(durationMs),
    ok,
    recordedAt: new Date().toISOString()
  };
  window.phylosaurPerformance.push(entry);
  if (window.phylosaurPerformance.length > 100) window.phylosaurPerformance.shift();
  console.debug(`[Phylosaur performance] ${action}: ${entry.durationMs} ms${ok ? '' : ' (failed)'}`);
}

window.getPhylosaurPerformanceSummary = function() {
  const groups = new Map();
  window.phylosaurPerformance.forEach(entry => {
    if (!groups.has(entry.action)) groups.set(entry.action, []);
    groups.get(entry.action).push(entry.durationMs);
  });
  return Array.from(groups, ([action, durations]) => ({
    action,
    requests: durations.length,
    averageMs: Math.round(durations.reduce((sum, value) => sum + value, 0) / durations.length),
    fastestMs: Math.min(...durations),
    slowestMs: Math.max(...durations)
  }));
};

function getAnalyticsVisitorId() {
  const storageKey = PHYLOSAUR_STORAGE_KEYS.visitorId;
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

const gameSessionCredentials = new Map();

function getGameSessionToken(sessionId) {
  if (typeof sessionId !== "string" || !sessionId) return null;
  if (gameSessionCredentials.has(sessionId)) return gameSessionCredentials.get(sessionId);
  try {
    const token = localStorage.getItem(`${PHYLOSAUR_STORAGE_KEYS.sessionCredentialPrefix}${sessionId}`);
    if (token && /^v1\.[A-Za-z0-9_-]{43}$/.test(token)) {
      gameSessionCredentials.set(sessionId, token);
      return token;
    }
  } catch { /* In-memory credentials still work when storage is unavailable. */ }
  return null;
}

function rememberGameSessionToken(data) {
  if (typeof data?.sessionId !== "string" || typeof data?.sessionToken !== "string"
      || !/^v1\.[A-Za-z0-9_-]{43}$/.test(data.sessionToken)) return;
  gameSessionCredentials.set(data.sessionId, data.sessionToken);
  try {
    localStorage.setItem(`${PHYLOSAUR_STORAGE_KEYS.sessionCredentialPrefix}${data.sessionId}`, data.sessionToken);
  } catch { /* Keep playing with the credential held in memory. */ }
}

function withGameSessionCredentials(payload) {
  const authenticated = { ...payload };
  const token = getGameSessionToken(payload.sessionId);
  if (token && authenticated.sessionToken === undefined) authenticated.sessionToken = token;
  if (Array.isArray(payload.proofSessionIds)) {
    authenticated.sessionCredentials = Object.fromEntries(payload.proofSessionIds
      .map(id => [id, getGameSessionToken(id)]).filter(([, value]) => value));
  }
  return authenticated;
}

async function callGameApi(action, payload = {}) {
  const requestStartedAt = performance.now();
  const controller = new AbortController();
  let timedOut = false;
  let timeout;
  // One deadline covers auth lookup, headers and the complete response body.
  // Race explicitly so even a stalled adapter that ignores abort cannot hold UI locks.
  const deadline = new Promise((_, reject) => {
    timeout = setTimeout(() => {
      timedOut = true;
      reject(new Error(t("api.timeout")));
      controller.abort();
    }, GAME_API_TIMEOUT_MS);
  });
  let response;
  let data;

  try {
    const request = (async () => {
      const { data: { session } } = await sb.auth.getSession();
      if (controller.signal.aborted) throw new Error(t("api.timeout"));
      const accessToken = session?.access_token || SUPABASE_ANON_KEY;
      const response = await fetch(GAME_API_URL, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'apikey': SUPABASE_ANON_KEY,
          'Authorization': `Bearer ${accessToken}`
        },
        body: JSON.stringify({ action, visitorId: getAnalyticsVisitorId(), ...withGameSessionCredentials(payload) }),
        signal: controller.signal
      });
      if (controller.signal.aborted) throw new Error(t("api.timeout"));
      let data;
      try {
        data = await response.json();
      } catch (error) {
        if (controller.signal.aborted || error?.name === 'AbortError') throw error;
        data = { ok: false, error: t('api.invalidResponse') };
      }
      return { response, data };
    })();
    ({ response, data } = await Promise.race([request, deadline]));
  } catch (error) {
    recordGameApiPerformance(action, performance.now() - requestStartedAt, false);
    if (timedOut || error?.name === 'AbortError') {
      throw new Error(t('api.timeout'));
    }
    if (navigator.onLine === false) {
      throw new Error(t('api.offline'));
    }
    throw new Error(t('api.unreachable'));
  } finally {
    clearTimeout(timeout);
  }

  recordGameApiPerformance(action, performance.now() - requestStartedAt, response.ok && data?.ok);

  if (!response.ok || !data?.ok) {
    const serverMessage = data?.error || '';
    const apiError = new Error(getLocalizedGameApiError(response.status, serverMessage));
    apiError.status = response.status;
    apiError.data = data;
    apiError.serverMessage = serverMessage;
    throw apiError;
  }

  rememberGameSessionToken(data);
  return data;
}

let analyticsAccessRequest = null;
let analyticsAccessOwnerId = null;

async function initializeAnalyticsAccess() {
  const ownerId = currentUserId;
  if (!ownerId) {
    analyticsAccessRequest = null;
    analyticsAccessOwnerId = null;
    analyticsAccessChecked = true;
    isAnalyticsAdmin = false;
    return false;
  }
  if (analyticsAccessChecked && analyticsAccessOwnerId === ownerId) return isAnalyticsAdmin;
  if (analyticsAccessRequest?.ownerId === ownerId) return analyticsAccessRequest.promise;

  analyticsAccessChecked = false;
  isAnalyticsAdmin = false;
  const request = { ownerId, promise: null };
  analyticsAccessRequest = request;
  const isCurrentRequest = () => analyticsAccessRequest === request && currentUserId === ownerId;
  request.promise = (async () => {
    try {
      const data = await callGameApi('analytics_access');
      if (!isCurrentRequest()) return false;
      isAnalyticsAdmin = data.allowed === true;
      analyticsAccessOwnerId = ownerId;
      analyticsAccessChecked = true;
      return isAnalyticsAdmin;
    } catch (error) {
      if (isCurrentRequest()) {
        isAnalyticsAdmin = false;
        analyticsAccessChecked = false;
      }
      return false;
    } finally {
      if (analyticsAccessRequest === request) analyticsAccessRequest = null;
    }
  })();
  return request.promise;
}

function getStoredGameSessionIds(limit = 10) {
  const sessionIds = [];

  for (let index = 0; index < localStorage.length; index++) {
    const key = localStorage.key(index);
    if (!key?.startsWith('phylosaur-session:')) continue;

    const sessionId = localStorage.getItem(key);
    if (sessionId && !sessionIds.includes(sessionId)) {
      sessionIds.push(sessionId);
    }
  }

  return sessionIds.slice(0, Math.max(1, Number(limit) || 10));
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
  const normalizedName = String(cladeName || '').trim();
  if (!normalizedName) return null;
  const wikiLanguage = currentLocale === 'pt-BR' ? 'pt' : currentLocale === 'es' ? 'es' : 'en';
  const cacheKey = `${wikiLanguage}:${normalizedName.toLowerCase()}`;
  if (wikipediaInfoCache.has(cacheKey)) return wikipediaInfoCache.get(cacheKey);

  const request = (async () => {
    const fetchFromWikipedia = async language => {
      const endpoint = `https://${language}.wikipedia.org/w/api.php`;
      const searchRes = await fetch(
        `${endpoint}?action=query&list=search&srsearch=${encodeURIComponent(normalizedName)}&format=json&origin=*`
      );
      if (!searchRes.ok) return null;
      const searchData = await searchRes.json();
      if (!searchData.query?.search?.length) return null;

      const pageTitle = searchData.query.search[0].title;
      const pageUrl = `https://${language}.wikipedia.org/wiki/${encodeURIComponent(pageTitle.replace(/ /g, '_'))}`;
      const extractRes = await fetch(
        `${endpoint}?action=query&titles=${encodeURIComponent(pageTitle)}&prop=pageimages|extracts&format=json&pithumbsize=300&exintro=1&explaintext=1&origin=*`
      );
      if (!extractRes.ok) return null;
      const extractData = await extractRes.json();
      const pages = extractData.query?.pages || {};
      const page = pages[Object.keys(pages)[0]];
      if (!page || page.missing !== undefined) return null;

      return {
        title: pageTitle,
        url: pageUrl,
        image: page.thumbnail?.source || null,
        description: page.extract || null,
        language
      };
    };

    try {
      return await fetchFromWikipedia(wikiLanguage)
        || (wikiLanguage === 'en' ? null : await fetchFromWikipedia('en'));
    } catch (e) {
      console.error('Wiki info error:', e);
      wikipediaInfoCache.delete(cacheKey);
      return null;
    }
  })();

  wikipediaInfoCache.set(cacheKey, request);
  return request;
}

async function fetchWikimediaImage(taxonName) {
  const cacheKey = String(taxonName || '').trim().toLowerCase();
  if (!cacheKey) return null;
  if (wikimediaImageCache.has(cacheKey)) return wikimediaImageCache.get(cacheKey);

  const request = (async () => {
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
      wikimediaImageCache.delete(cacheKey);
      return null;
    }
  })();

  wikimediaImageCache.set(cacheKey, request);
  return request;
}
