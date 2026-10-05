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
    // Titles select candidates; only a matching scientific name confirms identity.
    // Never use the first full-text search result as an encyclopedia summary.
    const taxonName = normalizedName.normalize('NFC').toLowerCase();
    const readJson = async url => {
      const controller = new AbortController();
      let timeout;
      try {
        return await Promise.race([
          (async () => {
            const response = await fetch(url, { signal: controller.signal });
            if (!response.ok) return null;
            return await response.json();
          })(),
          new Promise(resolve => {
            timeout = setTimeout(() => { controller.abort(); resolve(null); }, 6000);
          })
        ]);
      } catch (error) {
        console.warn('Encyclopedia request unavailable:', error);
        return null;
      } finally {
        clearTimeout(timeout);
      }
    };
    const loadPages = async (language, titles) => {
      const parameters = new URLSearchParams({
        action: 'query', titles: titles.join('|'), redirects: '1',
        prop: 'pageimages|extracts|pageprops|langlinks',
        pithumbsize: '300', exintro: '1', explaintext: '1',
        lllang: wikiLanguage, lllimit: '1', format: 'json', origin: '*'
      });
      const data = await readJson(`https://${language}.wikipedia.org/w/api.php?${parameters}`);
      return Object.values(data?.query?.pages || {}).filter(page =>
        page.missing === undefined && page.invalid === undefined && page.ns === 0
        && !Object.hasOwn(page.pageprops || {}, 'disambiguation')
        && /^Q\d+$/.test(page.pageprops?.wikibase_item || ''));
    };
    const confirmTaxon = async pages => {
      const ids = [...new Set(pages.map(page => page.pageprops.wikibase_item))];
      if (!ids.length) return null;
      const parameters = new URLSearchParams({
        action: 'wbgetentities', ids: ids.join('|'), props: 'claims',
        format: 'json', origin: '*'
      });
      const data = await readJson(`https://www.wikidata.org/w/api.php?${parameters}`);
      return pages.find(page =>
        (data?.entities?.[page.pageprops.wikibase_item]?.claims?.P225 || []).some(claim =>
          claim.rank !== 'deprecated' && claim.mainsnak?.snaktype === 'value'
          && String(claim.mainsnak.datavalue?.value || '')
            .trim().normalize('NFC').toLowerCase() === taxonName)) || null;
    };
    const toSummary = (page, language) => page?.extract?.trim() ? {
      title: page.title,
      url: `https://${language}.wikipedia.org/wiki/${encodeURIComponent(page.title.replace(/ /g, '_'))}`,
      image: page.thumbnail?.source || null,
      description: page.extract,
      language,
      wikidataId: page.pageprops.wikibase_item,
      isLanguageFallback: language !== wikiLanguage
    } : null;

    // A controlled disambiguation title covers genera such as Balaur, without
    // admitting unrelated search results. Unconfirmed synonyms fail closed.
    const englishPages = await loadPages('en', [normalizedName, `${normalizedName} (dinosaur)`]);
    const canonical = await confirmTaxon(englishPages);
    if (wikiLanguage === 'en') return toSummary(canonical, 'en');

    if (canonical) {
      const linkedTitle = canonical.langlinks?.find(link => link.lang === wikiLanguage)?.['*'];
      if (linkedTitle) {
        const localPages = await loadPages(wikiLanguage, [linkedTitle]);
        const localPage = localPages.find(page =>
          page.pageprops.wikibase_item === canonical.pageprops.wikibase_item);
        const localized = toSummary(localPage, wikiLanguage);
        if (localized) return localized;
      }
      return toSummary(canonical, 'en');
    }

    // English may be unavailable. An exact local article is still acceptable
    // only after the same scientific-name check; no general search fallback.
    const localPages = await loadPages(wikiLanguage, [normalizedName]);
    return toSummary(await confirmTaxon(localPages), wikiLanguage);
  })();

  wikipediaInfoCache.set(cacheKey, request);
  // Do not retain unavailable summaries after a transient API failure.
  void request.then(result => {
    if (!result && wikipediaInfoCache.get(cacheKey) === request) wikipediaInfoCache.delete(cacheKey);
  }, () => {
    if (wikipediaInfoCache.get(cacheKey) === request) wikipediaInfoCache.delete(cacheKey);
  });
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
