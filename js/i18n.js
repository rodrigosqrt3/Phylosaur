// ═══════════════════════════════════════════════
// LOCALIZATION
// English is the source language and universal fallback. Scientific genus
// and clade names remain unchanged in every locale.
// ═══════════════════════════════════════════════
const PHYLOSAUR_SUPPORTED_LOCALES = Object.freeze(['en', 'pt-BR', 'es', 'ja', 'fr', 'de']);
const PHYLOSAUR_DEFAULT_LOCALE = 'en';

// Catalogs are loaded before this file from js/locales/<code>.js and register themselves on
// window.PHYLOSAUR_CATALOGS. A locale that is missing or incomplete falls back to English.
const PHYLOSAUR_MESSAGES = Object.freeze(Object.fromEntries(
  PHYLOSAUR_SUPPORTED_LOCALES.map(code => [code, (window.PHYLOSAUR_CATALOGS || {})[code] || {}])
));

function normalizePhylosaurLocale(value) {
  const locale = String(value || '').trim().toLowerCase();
  if (locale === 'pt' || locale.startsWith('pt-')) return 'pt-BR';
  if (locale === 'es' || locale.startsWith('es-')) return 'es';
  if (locale === 'ja' || locale.startsWith('ja-')) return 'ja';
  if (locale === 'fr' || locale.startsWith('fr-')) return 'fr';
  if (locale === 'de' || locale.startsWith('de-')) return 'de';
  return 'en';
}

function detectPhylosaurLocale() {
  const queryLocale = new URLSearchParams(window.location.search).get('lang');
  if (queryLocale) return normalizePhylosaurLocale(queryLocale);
  try {
    const saved = localStorage.getItem(PHYLOSAUR_STORAGE_KEYS.language);
    if (saved) return normalizePhylosaurLocale(saved);
  } catch (_error) {}
  const preferred = navigator.languages?.[0] || navigator.language || PHYLOSAUR_DEFAULT_LOCALE;
  return normalizePhylosaurLocale(preferred);
}

let currentLocale = detectPhylosaurLocale();

function t(key, replacements = {}) {
  const fallback = PHYLOSAUR_MESSAGES.en[key] ?? key;
  const message = PHYLOSAUR_MESSAGES[currentLocale]?.[key] ?? fallback;
  return String(message).replace(/\{([A-Za-z0-9_]+)\}/g, (_match, name) =>
    replacements[name] ?? `{${name}}`
  );
}

function getPhylosaurLocaleTag() {
  return currentLocale === 'pt-BR' ? 'pt-BR' : currentLocale;
}

function formatPhylosaurDate(value, options = {}) {
  return new Intl.DateTimeFormat(getPhylosaurLocaleTag(), options).format(value);
}

function changeLanguage(locale) {
  const normalized = normalizePhylosaurLocale(locale);
  try {
    localStorage.setItem(PHYLOSAUR_STORAGE_KEYS.language, normalized);
  } catch (_error) {}
  const url = new URL(window.location.href);
  url.searchParams.delete('lang');
  const nextUrl = `${url.pathname}${url.search}${url.hash}`;
  window.history.replaceState(window.history.state, document.title, nextUrl);
  window.location.reload();
}

const PHYLOSAUR_ENGLISH_TO_KEY = new Map(
  Object.entries(PHYLOSAUR_MESSAGES.en).map(([key, value]) => [value, key])
);

function translateKnownText(value) {
  const key = PHYLOSAUR_ENGLISH_TO_KEY.get(value);
  return key ? t(key) : value;
}

function localizeTextNode(node) {
  if (!node?.nodeValue || currentLocale === 'en') return;
  const match = node.nodeValue.match(/^(\s*)(.*?)(\s*)$/s);
  if (!match || !match[2]) return;
  if (node.parentElement?.closest('[data-localization-ignore]')) return;
  const translated = translateKnownText(match[2]);
  if (translated !== match[2]) node.nodeValue = `${match[1]}${translated}${match[3]}`;
}

function localizeElement(element) {
  if (!(element instanceof Element)) return;
  const htmlKey = element.dataset.i18nHtml;
  if (htmlKey) element.innerHTML = t(htmlKey);
  const key = element.dataset.i18n;
  if (key) element.textContent = t(key);
  ['aria-label', 'title', 'placeholder'].forEach(attribute => {
    const value = element.getAttribute(attribute);
    if (!value) return;
    const translated = translateKnownText(value);
    if (translated !== value) element.setAttribute(attribute, translated);
  });
}

function localizeDocument(root = document) {
  if (root instanceof Element) localizeElement(root);
  const walker = document.createTreeWalker(root, NodeFilter.SHOW_ELEMENT | NodeFilter.SHOW_TEXT);
  let node;
  while ((node = walker.nextNode())) {
    if (node.nodeType === Node.TEXT_NODE) localizeTextNode(node);
    else localizeElement(node);
  }
}

// The raw HTML has no canonical link on purpose: a static one would point every
// ?lang= variant at the English page. Each supported URL declares itself instead.
function syncPhylosaurCanonical() {
  const page = document.querySelector('link[rel="alternate"][hreflang="x-default"]');
  if (!page) return;
  const canonicalUrl = new URL(page.href);
  const requested = new URLSearchParams(window.location.search).get('lang');
  if (PHYLOSAUR_SUPPORTED_LOCALES.includes(requested)) canonicalUrl.searchParams.set('lang', requested);
  let canonical = document.querySelector('link[rel="canonical"]');
  if (!canonical) {
    canonical = document.createElement('link');
    canonical.rel = 'canonical';
    document.head.appendChild(canonical);
  }
  canonical.href = canonicalUrl.href;
  document.querySelector('meta[property="og:url"]')?.setAttribute('content', canonicalUrl.href);
}

let phylosaurLocalizationObserver = null;

function initializeI18n() {
  document.documentElement.lang = getPhylosaurLocaleTag();
  if (document.title === PHYLOSAUR_MESSAGES.en['app.metaTitle']) {
    document.title = t('app.metaTitle');
    document.querySelector('meta[name="description"]')?.setAttribute('content', t('app.metaDescription'));
  }
  syncPhylosaurCanonical();
  const selector = document.getElementById('language-select');
  if (selector) {
    selector.value = currentLocale;
    selector.setAttribute('aria-label', t('language.label'));
  }
  localizeDocument(document);
  phylosaurLocalizationObserver?.disconnect();
  phylosaurLocalizationObserver = new MutationObserver(mutations => {
    mutations.forEach(mutation => mutation.addedNodes.forEach(node => {
      if (node.nodeType === Node.TEXT_NODE) localizeTextNode(node);
      else if (node.nodeType === Node.ELEMENT_NODE) localizeDocument(node);
    }));
  });
  phylosaurLocalizationObserver.observe(document.body, { childList: true, subtree: true });
}