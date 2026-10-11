// ═══════════════════════════════════════════════
// UI AND MODALS
// ═══════════════════════════════════════════════
function setHeaderControls(screen) {
    if (screen !== 'challenge' && typeof stopChallengeStatusPolling === 'function') {
      stopChallengeStatusPolling();
    }
    if (screen !== 'museum' && typeof releaseMuseumViewResources === 'function') {
      releaseMuseumViewResources();
    } else if (screen !== 'museum' && typeof stopMuseumCardMediaLoading === 'function') {
      stopMuseumCardMediaLoading();
    }
    const controls = document.getElementById('header-controls');
    if (!controls) return;

    const glyphs = {
      museum: '<path d="M3 9.2 12 4l9 5.2"/><path d="M4.5 9.8h15"/><path d="M6.5 12v5.5M10.2 12v5.5M13.8 12v5.5M17.5 12v5.5"/><path d="M4 20h16"/>',
      stats: '<path d="M6.5 18v-2M12 18v-7M17.5 18V6" stroke-width="3.2"/>',
      analytics: '<path d="M4 18l5-6 4 3 7-8"/><path d="M15 7h5v5"/>',
      account: '<circle cx="12" cy="8.5" r="3.5"/><path d="M5 20c.8-3.6 3.6-5.5 7-5.5s6.2 1.9 7 5.5"/>',
      back: '<path d="M14.5 5.5 8 12l6.5 6.5"/>',
    };
    const glyph = name => `<svg class="header-glyph" viewBox="0 0 24 24" aria-hidden="true" focusable="false">${glyphs[name]}</svg>`;
    const iconBtn = (name, label, action = '', extra = '') =>
      `<button class="btn-hint btn-header btn-header-icon btn-header-${name}" type="button"${action ? ` onclick="${action}"` : ''} title="${escapeHtml(label)}"${extra}>${glyph(name)}<span class="visually-hidden">${escapeHtml(label)}</span></button>`;
    const backBtn = (action, label) => iconBtn('back', label, action);

    const statsBtn = currentUser ? iconBtn('stats', t('nav.stats'), 'showStatsDashboard()') : '';
    const analyticsBtn = isAnalyticsAdmin ? iconBtn('analytics', t('nav.analytics'), 'showAnalyticsDashboard()') : '';

    const safeCurrentUser = escapeHtml(currentUser);
    const signOutLabel = t('nav.signOut', { name: safeCurrentUser });
    const accountInitial = escapeHtml(Array.from(String(currentUser || '').trim())[0]?.toLocaleUpperCase() || '?');
    const logoutBtn = currentUser
      ? `<button class="btn-hint btn-header btn-header-icon btn-account btn-header-avatar" type="button" onclick="logout()" title="${signOutLabel}"><span class="header-avatar" aria-hidden="true">${accountInitial}</span><span class="visually-hidden">${signOutLabel}</span></button>`
      : '';
    const signInBtn = `<button class="btn-hint btn-header btn-with-icon" onclick="showLoginModal()">${glyph('account')}<span>${t('nav.signIn')}</span></button>`;

    const map = {
      'login':        '',
      'difficulty':   iconBtn('museum', t('nav.museum'), 'showMuseum()') + analyticsBtn + (currentUser ? statsBtn + logoutBtn : signInBtn),
      'game':         backBtn("navigateToAppRoute('/')", t('nav.levels')) + (currentUser ? statsBtn : signInBtn),
      'stats':        backBtn("navigateBackOrHome('/')", t('nav.back')),
      'museum':       backBtn("navigateBackOrHome('/')", t('nav.back')),
      'about':        backBtn("navigateBackOrHome('/')", t('nav.back')),
      'practice-menu':backBtn("navigateToAppRoute('/')", t('nav.levels')),
      'practice':     backBtn("navigateToAppRoute('/')", t('nav.levels')),
      'friends':      backBtn("navigateBackOrHome('/')", t('nav.back')),
      'challenge':    backBtn("navigateBackOrHome('/friends')", t('nav.friends')),
      'analytics':    backBtn("navigateBackOrHome('/')", t('nav.back')),
    };

    // Every screen reserves the same complete menu, even when it only shows
    // Back or has no actions. Account/permission updates cannot resize the shell.
    controls.classList.toggle('stable-header-controls', true);
    const reserved = ' tabindex="-1" disabled';
    const sizingMenu = `
        <div class="header-controls-reserve" aria-hidden="true" inert>
          ${iconBtn('museum', t('nav.museum'), '', reserved)}
          ${iconBtn('analytics', t('nav.analytics'), '', reserved)}
          ${iconBtn('stats', t('nav.stats'), '', reserved)}
          <button class="btn-hint btn-header btn-header-icon header-account-reserve" type="button" tabindex="-1" disabled>${glyph('account')}<span class="visually-hidden">${t('common.player')}</span></button>
        </div>`;
    controls.innerHTML = sizingMenu + `<div class="header-controls-visible">${map[screen] || ''}</div>`;
}

function applyTheme(theme, { persist = true } = {}) {
    currentTheme = theme === 'light' ? 'light' : 'dark';
    document.documentElement.dataset.theme = currentTheme;
    document.body.classList.toggle('light-mode', currentTheme === 'light');
    document.querySelector('meta[name="theme-color"]')
        ?.setAttribute('content', currentTheme === 'light' ? '#f5f0e8' : '#2d2318');
    if (persist) {
        try {
            localStorage.setItem(PHYLOSAUR_STORAGE_KEYS.theme, currentTheme);
        } catch (_error) {}
    }
    updateThemeToggleState();
}

function toggleTheme() {
    applyTheme(currentTheme === 'dark' ? 'light' : 'dark');
}

function updateThemeToggleState() {
    const toggle = document.getElementById('theme-toggle');
    if (!toggle) return;

    const isLight = currentTheme === 'light';
    toggle.setAttribute('aria-pressed', String(isLight));
    toggle.setAttribute('aria-label', isLight ? t('theme.toDark') : t('theme.toLight'));
}

function escapeAppStateText(value) {
    return escapeHtml(value);
}

function renderAppState(message, { type = 'loading', detail = '', compact = false } = {}) {
    const safeType = ['loading', 'error', 'empty'].includes(type) ? type : 'loading';
    const role = safeType === 'error' ? 'alert' : 'status';
    return `<div class="app-state app-state-${safeType}${compact ? ' app-state-compact' : ''}"
                 role="${role}" aria-live="${safeType === 'error' ? 'assertive' : 'polite'}">
        <span class="app-state-icon" aria-hidden="true"></span>
        <div class="app-state-message">${escapeAppStateText(message)}</div>
        ${detail ? `<div class="app-state-detail">${escapeAppStateText(detail)}</div>` : ''}
    </div>`;
}

function focusAppScreenHeading(selector = '.screen-title') {
    queueMicrotask(() => {
        if (document.querySelector('.modal-overlay, .tutorial-overlay, [aria-modal="true"]')) return;
        const appContent = document.getElementById('app-content');
        const heading = appContent?.querySelector(selector);
        if (!heading || !heading.isConnected) return;

        heading.setAttribute('tabindex', '-1');
        try {
            heading.focus({ preventScroll: true });
        } catch (_error) {
            heading.focus();
        }

        const label = heading.textContent?.replace(/\s+/g, ' ').trim();
        if (label) document.title = `${label} - Phylosaur`;
    });
}

const GLASS_SELECT_CHEVRON = '<svg class="glass-select-chevron" viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M7 10l5 5 5-5"/></svg>';
const nativeSelectValue = Object.getOwnPropertyDescriptor(HTMLSelectElement.prototype, 'value');
let glassSelectCount = 0;
let openGlassSelectState = null;
let glassSelectObserver = null;

function enhanceGlassSelect(select) {
    if (!(select instanceof HTMLSelectElement) || select.dataset.glassSelect || select.multiple) return;
    select.dataset.glassSelect = 'true';
    const baseId = select.id || `glass-select-${++glassSelectCount}`;
    const label = select.id ? document.querySelector(`label[for="${CSS.escape(select.id)}"]`) : null;
    const trigger = document.createElement('button');
    trigger.type = 'button';
    trigger.className = 'glass-select-trigger';
    trigger.id = `${baseId}-trigger`;
    trigger.setAttribute('role', 'combobox');
    trigger.setAttribute('aria-haspopup', 'listbox');
    trigger.setAttribute('aria-expanded', 'false');
    trigger.setAttribute('aria-controls', `${baseId}-menu`);
    if (select.getAttribute('aria-label')) {
        trigger.setAttribute('aria-label', select.getAttribute('aria-label'));
    } else if (label) {
        label.id ||= `${baseId}-label`;
        trigger.setAttribute('aria-labelledby', label.id);
    }
    if (select.hasAttribute('data-localization-ignore')) trigger.setAttribute('data-localization-ignore', '');
    trigger.innerHTML = `<span class="glass-select-value"></span>${GLASS_SELECT_CHEVRON}`;

    const refresh = () => {
        trigger.querySelector('.glass-select-value').textContent = select.selectedOptions[0]?.textContent.trim() || '';
        trigger.disabled = select.disabled;
    };
    Object.defineProperty(select, 'value', {
        configurable: true,
        get() { return nativeSelectValue.get.call(this); },
        set(value) { nativeSelectValue.set.call(this, value); refresh(); },
    });
    select.addEventListener('change', refresh);
    select.addEventListener('focus', () => trigger.focus());
    select.classList.add('glass-select-native');
    select.tabIndex = -1;
    select.setAttribute('aria-hidden', 'true');
    select.after(trigger);
    refresh();

    trigger.addEventListener('click', () => {
        if (openGlassSelectState?.trigger === trigger) closeGlassSelect();
        else openGlassSelect(select, trigger);
    });
    trigger.addEventListener('keydown', event => handleGlassSelectKey(event, select, trigger));
}

function openGlassSelect(select, trigger) {
    closeGlassSelect();
    const menu = document.createElement('div');
    menu.className = 'glass-select-menu';
    menu.id = trigger.getAttribute('aria-controls');
    menu.setAttribute('role', 'listbox');
    ['aria-label', 'aria-labelledby', 'data-localization-ignore'].forEach(name => {
        if (trigger.hasAttribute(name)) menu.setAttribute(name, trigger.getAttribute(name));
    });
    [...select.options].forEach((option, index) => {
        const item = document.createElement('div');
        item.className = 'glass-select-option';
        item.id = `${menu.id}-option-${index}`;
        item.dataset.index = String(index);
        item.setAttribute('role', 'option');
        item.setAttribute('aria-selected', String(option.selected));
        if (option.disabled) item.setAttribute('aria-disabled', 'true');
        item.textContent = option.textContent.trim();
        menu.appendChild(item);
    });
    menu.addEventListener('pointerdown', event => event.preventDefault());
    menu.addEventListener('click', event => {
        const item = event.target.closest('.glass-select-option');
        if (item && !item.hasAttribute('aria-disabled')) chooseGlassSelectOption(Number(item.dataset.index));
    });
    menu.addEventListener('pointermove', event => {
        const item = event.target.closest('.glass-select-option');
        if (item && !item.hasAttribute('aria-disabled')) setGlassSelectActive(Number(item.dataset.index), false);
    });
    document.body.appendChild(menu);
    openGlassSelectState = { select, trigger, menu, active: -1 };
    trigger.setAttribute('aria-expanded', 'true');
    positionGlassSelect();
    setGlassSelectActive(Math.max(0, select.selectedIndex));
}

function closeGlassSelect() {
    const state = openGlassSelectState;
    if (!state) return;
    openGlassSelectState = null;
    state.menu.remove();
    state.trigger.setAttribute('aria-expanded', 'false');
    state.trigger.removeAttribute('aria-activedescendant');
}

function positionGlassSelect() {
    const state = openGlassSelectState;
    if (!state) return;
    if (!state.trigger.isConnected) {
        closeGlassSelect();
        return;
    }
    const rect = state.trigger.getBoundingClientRect();
    const width = Math.min(Math.max(rect.width, 190), window.innerWidth - 16);
    const left = Math.max(8, Math.min(rect.right - width, rect.left, window.innerWidth - width - 8));
    const below = window.innerHeight - rect.bottom - 14;
    const above = rect.top - 14;
    const opensUp = below < 220 && above > below;
    const style = state.menu.style;
    style.left = `${left}px`;
    style.width = `${width}px`;
    style.maxHeight = `${Math.max(120, Math.min(320, opensUp ? above : below))}px`;
    style.top = opensUp ? '' : `${rect.bottom + 6}px`;
    style.bottom = opensUp ? `${window.innerHeight - rect.top + 6}px` : '';
    state.menu.classList.toggle('opens-up', opensUp);
}

function setGlassSelectActive(index, scroll = true) {
    const state = openGlassSelectState;
    const items = state?.menu.querySelectorAll('.glass-select-option');
    if (!items?.[index]) return;
    items[state.active]?.classList.remove('is-active');
    state.active = index;
    items[index].classList.add('is-active');
    state.trigger.setAttribute('aria-activedescendant', items[index].id);
    if (scroll) items[index].scrollIntoView({ block: 'nearest' });
}

function moveGlassSelectActive(step) {
    const state = openGlassSelectState;
    const options = [...state.select.options];
    let index = state.active;
    for (let tries = 0; tries < options.length; tries++) {
        index = Math.min(options.length - 1, Math.max(0, index + step));
        if (!options[index].disabled) break;
    }
    setGlassSelectActive(index);
}

function chooseGlassSelectOption(index) {
    const state = openGlassSelectState;
    if (!state) return;
    const { select, trigger } = state;
    closeGlassSelect();
    trigger.focus();
    if (select.selectedIndex === index || select.options[index]?.disabled) return;
    select.selectedIndex = index;
    select.dispatchEvent(new Event('change', { bubbles: true }));
}

function handleGlassSelectKey(event, select, trigger) {
    const isOpen = openGlassSelectState?.trigger === trigger;
    if (!isOpen) {
        if (['ArrowDown', 'ArrowUp', 'Enter', ' '].includes(event.key)) {
            event.preventDefault();
            openGlassSelect(select, trigger);
        }
        return;
    }
    const lastIndex = select.options.length - 1;
    if (event.key === 'ArrowDown') moveGlassSelectActive(1);
    else if (event.key === 'ArrowUp') moveGlassSelectActive(-1);
    else if (event.key === 'Home') setGlassSelectActive(0);
    else if (event.key === 'End') setGlassSelectActive(lastIndex);
    else if (event.key === 'Enter' || event.key === ' ') chooseGlassSelectOption(openGlassSelectState.active);
    else if (event.key === 'Escape') {
        event.stopPropagation();
        closeGlassSelect();
    } else if (event.key === 'Tab') {
        closeGlassSelect();
        return;
    } else if (event.key.length === 1 && /\S/.test(event.key)) {
        const letter = event.key.toLocaleLowerCase();
        const options = [...select.options];
        const start = openGlassSelectState.active + 1;
        const match = [...options.slice(start), ...options.slice(0, start)]
            .find(option => !option.disabled && option.textContent.trim().toLocaleLowerCase().startsWith(letter));
        if (match) setGlassSelectActive(match.index);
    } else {
        return;
    }
    event.preventDefault();
}

function watchGlassSelects() {
    document.querySelectorAll('select').forEach(enhanceGlassSelect);
    if (glassSelectObserver) return;
    glassSelectObserver = new MutationObserver(mutations => mutations.forEach(mutation =>
        mutation.addedNodes.forEach(node => {
            if (node.nodeType !== Node.ELEMENT_NODE) return;
            if (node.tagName === 'SELECT') enhanceGlassSelect(node);
            else node.querySelectorAll('select').forEach(enhanceGlassSelect);
        })
    ));
    glassSelectObserver.observe(document.body, { childList: true, subtree: true });
    document.addEventListener('pointerdown', event => {
        const state = openGlassSelectState;
        if (state && !state.menu.contains(event.target) && !state.trigger.contains(event.target)) closeGlassSelect();
    });
    window.addEventListener('resize', positionGlassSelect);
    window.addEventListener('scroll', positionGlassSelect, true);
    window.addEventListener('hashchange', closeGlassSelect);
}

function isTopAppOverlay(overlay) {
    const overlays = document.querySelectorAll('[aria-modal="true"]');
    return overlay.isConnected && overlays[overlays.length - 1] === overlay;
}

function showModal(options) {
    return new Promise((resolve) => {
    const overlay = document.createElement('div');
    overlay.className = 'modal-overlay';
    overlay.dataset.appModal = 'true';
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-modal', 'true');
    overlay.setAttribute('aria-label', options.title || 'Dialog');
    
    const box = document.createElement('div');
    box.className = 'modal-box';
    box.tabIndex = -1;

    const previouslyFocused = document.activeElement;
    const previousBodyOverflow = document.body.style.overflow;
    
    let html = '<div class="modal-scroll-region">';
    
    if (options.title) {
        html += `<div class="modal-title">${options.title}</div>`;
    }
    
    if (options.message) {
        const messageId = `modal-message-${Date.now().toString(36)}${Math.random().toString(36).slice(2, 6)}`;
        overlay.setAttribute('aria-describedby', messageId);
        html += `<div class="modal-message" id="${messageId}">${options.message}</div>`;
    }
    
    if (options.info) {
        html += '<div class="modal-info">';
        options.info.forEach(item => {
        html += `
            <div class="modal-info-item">
            <span class="modal-info-label">${item.label}:</span>
            <span>${item.value}</span>
            </div>
        `;
        });
        html += '</div>';
    }

    html += '</div>';
    
    html += '<div class="modal-buttons">';
    
    if (options.buttons) {
        options.buttons.forEach((btn, index) => {
        const btnClass = btn.primary ? 'modal-btn-primary' : 'modal-btn-secondary';
        const autofocus = btn.autofocus ? ' data-autofocus="true"' : '';
        html += `<button class="modal-btn ${btnClass}" data-result="${btn.value}"${autofocus}>${btn.text}</button>`;
        });
    } else {
        html += `<button class="modal-btn modal-btn-primary" data-result="ok">${t('common.ok')}</button>`;
    }
    
    html += '</div>';
    
    box.innerHTML = html;
    overlay.appendChild(box);
    document.body.appendChild(overlay);
    document.body.style.overflow = 'hidden';

    let closed = false;
    let keyHandlerAttached = false;

    const closeModal = (result, { restoreFocus = true } = {}) => {
        if (closed) return;
        closed = true;
        clearTimeout(keyHandlerTimer);
        if (keyHandlerAttached) {
            document.removeEventListener('keydown', modalKeyHandler, true);
        }
        overlay.remove();
        document.body.style.overflow = previousBodyOverflow;
        if (restoreFocus && previouslyFocused instanceof HTMLElement && previouslyFocused.isConnected) {
            previouslyFocused.focus();
        }
        resolve(result);
    };

    const modalKeyHandler = event => {
        if (!isTopAppOverlay(overlay)) return;
        const buttons = box.querySelectorAll('.modal-btn');

        if (event.key === 'Tab') {
            const focusable = Array.from(box.querySelectorAll(
                'button:not([disabled]), a[href], input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex="-1"])'
            ));

            if (!focusable.length) {
                event.preventDefault();
                box.focus();
                return;
            }

            const first = focusable[0];
            const last = focusable[focusable.length - 1];
            if (event.shiftKey && document.activeElement === first) {
                event.preventDefault();
                last.focus();
            } else if (!event.shiftKey && document.activeElement === last) {
                event.preventDefault();
                first.focus();
            }
        } else if (event.key === 'Enter' && buttons.length === 1) {
            event.preventDefault();
            event.stopImmediatePropagation();
            closeModal(buttons[0].getAttribute('data-result'));
        } else if (event.key === 'Escape' && options.closeOnOverlay !== false) {
            event.preventDefault();
            event.stopImmediatePropagation();
            closeModal(null);
        }
    };

    // Attach after the event that opened the modal has finished propagating.
    const keyHandlerTimer = setTimeout(() => {
        if (closed) return;
        document.addEventListener('keydown', modalKeyHandler, true);
        keyHandlerAttached = true;
        if (!isTopAppOverlay(overlay)) return;
        const firstButton = box.querySelector('.modal-btn[data-autofocus]') || box.querySelector('.modal-btn');
        if (firstButton) firstButton.focus();
        else box.focus();
    }, 0);
    overlay.dismissAppOverlay = ({ restoreFocus = true } = {}) => closeModal(null, { restoreFocus });
    
    box.querySelectorAll('.modal-btn').forEach(btn => {
        btn.addEventListener('click', () => {
        closeModal(btn.getAttribute('data-result'));
        });
    });
    
    overlay.addEventListener('click', (e) => {
        if (e.target === overlay && options.closeOnOverlay !== false) {
        closeModal(null);
        }
    });
    });
}

function customAlert(title, message) {
    return showModal({
    title: title,
    message: message,
    buttons: [{ text: t('common.ok'), value: 'ok', primary: true }]
    });
}

function customConfirm(title, message, yesText = 'Yes', noText = 'No', { focusCancel = false } = {}) {
    return showModal({
    title: title,
    message: message,
    buttons: [
        { text: yesText, value: true, primary: true },
        { text: noText, value: false, primary: false, autofocus: focusCancel }
    ]
    });
}

function openImageLightbox(url, name, sourcePage = '', creditHtml = '') {
    const previousViewer = document.getElementById('image-lightbox');
    // Do not replace a covered viewer underneath a newer account/confirmation dialog.
    if (previousViewer && !isTopAppOverlay(previousViewer)) return;
    const previouslyFocused = previousViewer?.lightboxPreviouslyFocused || document.activeElement;
    previousViewer?.dismissAppOverlay({ restoreFocus: false });
    const previousBodyOverflow = document.body.style.overflow;
    const overlay = document.createElement('div');
    overlay.id = 'image-lightbox';
    overlay.className = 'image-lightbox';
    overlay.lightboxPreviouslyFocused = previouslyFocused;
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-modal', 'true');
    overlay.setAttribute('aria-label', t('media.imageViewer', { name }));
    overlay.tabIndex = -1;
    overlay.style.cssText = `
    position:fixed; top:0; left:0; right:0; bottom:0;
    background:rgba(0,0,0,0.92); z-index:10000;
    display:flex; flex-direction:column;
    align-items:center; justify-content:center;
    cursor:zoom-out; animation:fadeIn 0.2s ease;
    padding:20px;
    `;
    
    const fallbackCredit = `
        ${t('media.imageSource')} ${sourcePage ? `<a href="${escapeHtml(sourcePage)}" target="_blank" rel="noopener noreferrer"
            style="color:var(--color-muted); text-decoration:none;">Wikimedia Commons</a>` : 'Wikimedia Commons'}
    `;

    overlay.innerHTML = `
    <button class="image-lightbox-close museum-image-viewer-close" type="button"
        aria-label="${escapeHtml(t('museum.closeImage'))}">×</button>
    <img src="${escapeHtml(url)}" alt="${escapeHtml(name)}"
        style="max-width:90vw; max-height:min(80vh, calc(100dvh - 150px)); border-radius:8px;
                border:2px solid var(--border-subtle); box-shadow:0 8px 40px rgba(0,0,0,0.8);" />
    <div style="margin-top:16px; font-family:Georgia,serif; font-style:italic;
                color:var(--color-accent); font-size:1.1em; letter-spacing:1px;">${escapeHtml(name)}</div>
    <div style="margin-top:8px; font-size:0.82em; color:var(--border-subtle); letter-spacing:1px;">
        ${creditHtml || fallbackCredit}
    </div>
    <div style="margin-top:16px; color:var(--border-subtle); font-size:0.8em; letter-spacing:2px;">
        ${t('media.closeAnywhere')}
    </div>
    `;
    
    overlay.querySelectorAll('a').forEach(link => {
        link.addEventListener('click', event => event.stopPropagation());
    });

    let closed = false;
    const closeLightbox = ({ restoreFocus = true } = {}) => {
        if (closed) return;
        closed = true;
        const wasTop = isTopAppOverlay(overlay);
        const hadFocus = overlay.contains(document.activeElement);
        document.removeEventListener('keydown', lightboxKeyHandler, true);
        overlay.remove();
        document.body.style.overflow = previousBodyOverflow;
        if (restoreFocus && wasTop && hadFocus
            && previouslyFocused instanceof HTMLElement && previouslyFocused.isConnected) {
            previouslyFocused.focus({ preventScroll: true });
        }
    };

    const lightboxKeyHandler = event => {
        if (!isTopAppOverlay(overlay)) return;
        if (event.key === 'Escape') {
            event.preventDefault();
            event.stopImmediatePropagation();
            closeLightbox();
        } else if (event.key === 'Tab') {
            const controls = Array.from(overlay.querySelectorAll(
                'button:not([disabled]):not([hidden]), a[href]:not([hidden]), [tabindex="0"]:not([hidden])'
            ));
            const first = controls[0];
            const last = controls.at(-1);
            if (!first) {
                event.preventDefault();
                overlay.focus({ preventScroll: true });
            } else if (!controls.includes(document.activeElement)
                || (event.shiftKey && document.activeElement === first)
                || (!event.shiftKey && document.activeElement === last)) {
                event.preventDefault();
                (event.shiftKey ? last : first).focus({ preventScroll: true });
            }
        }
    };

    overlay.dismissAppOverlay = closeLightbox;
    overlay.addEventListener('click', () => {
        if (isTopAppOverlay(overlay)) closeLightbox();
    });
    document.body.appendChild(overlay);
    document.body.style.overflow = 'hidden';
    document.addEventListener('keydown', lightboxKeyHandler, true);
    overlay.querySelector('.image-lightbox-close').focus({ preventScroll: true });
}

let cladeInfoRequestId = 0;

function formatTaxonName(name) {
    const safeName = escapeChallengeHtml(name);
    const isGenus = [...(database || []), ...(fullDatabase || [])].some(dino => dino?.nome === name);
    return isGenus ? `<em>${safeName}</em>` : safeName;
}

async function showCladeInfo(cladeName, options = {}) {
    const infoDiv = document.getElementById('clade-info');
    if (!infoDiv || !cladeName) return;

    const requestId = ++cladeInfoRequestId;
    const heading = options.heading || t('tree.clade');
    infoDiv.innerHTML = `
    <div class="clade-info">
        <div class="loading">${t('tree.loadingInfo', { clade: escapeChallengeHtml(cladeName) })}</div>
    </div>
    `;

    const wikiInfo = await fetchWikipediaInfo(cladeName);
    if (requestId !== cladeInfoRequestId || !infoDiv.isConnected) return;

    if (!wikiInfo) {
    infoDiv.innerHTML = `
        <div class="clade-info">
        <h3>${escapeChallengeHtml(heading)}: ${formatTaxonName(cladeName)}</h3>
        <p class="clade-empty-copy">${t('tree.noEncyclopedia')}</p>
        </div>
    `;
    return;
    }

    let html = `<div class="clade-info"><h3>${escapeChallengeHtml(heading)}: ${formatTaxonName(cladeName)}</h3><div class="clade-content">`;
    
    if (wikiInfo.image) {
    html += `<img src="${escapeChallengeHtml(wikiInfo.image)}" alt="${escapeChallengeHtml(wikiInfo.title)}" class="clade-image" />`;
    }
    
    html += `
    <div class="clade-text">
        ${wikiInfo.description 
        ? `<p>${escapeChallengeHtml(wikiInfo.description)}</p>`
        : `<p class="clade-empty-copy">${t('tree.descriptionUnavailable')}</p>`
        }
        ${wikiInfo.isLanguageFallback
          ? `<p class="encyclopedia-language-note">${t('encyclopedia.englishFallback')}</p>` : ''}
        <a href="${escapeChallengeHtml(wikiInfo.url)}" target="_blank" rel="noopener" class="clade-link">${t('tree.encyclopedia')}</a>
    </div>
    `;
    
    html += '</div></div>';
    infoDiv.innerHTML = html;
}

async function updateCladeInfo() {
    const infoDiv = document.getElementById('clade-info');
    if (!infoDiv) return;

    const bestGuess = guesses.reduce((best, candidate) =>
        Number(candidate?.proximity?.matches || 0) > Number(best?.proximity?.matches || 0)
            ? candidate
            : best,
    null);
    const bestGuessDepth = Number(bestGuess?.proximity?.matches || 0);
    const bestGuessClade = bestGuess?.proximity?.lastCommonClade || null;

    const deepestHint = hintHistory.reduce((best, hint) => {
        if (!hint?.cladeName) return best;
        return Number(hint.depth || 0) > Number(best?.depth || 0) ? hint : best;
    }, null);
    const deepestHintDepth = Number(deepestHint?.depth || 0);

    const useHintClade = Boolean(deepestHint?.cladeName)
        && deepestHintDepth >= bestGuessDepth;
    const selectedClade = useHintClade ? deepestHint.cladeName : bestGuessClade;

    if (!selectedClade) {
        cladeInfoRequestId++;
        infoDiv.innerHTML = '';
        return;
    }

    await showCladeInfo(selectedClade, {
        heading: useHintClade ? t('tree.deepestRevealed') : t('tree.recentAncestor')
    });
}

function updateGuessHistory() {
    const historyDiv = document.getElementById('guess-history');

    if (!historyDiv) return;

    if (guesses.length === 0 && hintHistory.length === 0) {
        historyDiv.innerHTML = '';
        return;
    }

    const wasExpanded = Boolean(historyDiv.querySelector('details.guess-history-details')?.open);
    const latestGuessIndex = guesses.length - 1;
    const latestGuess = latestGuessIndex >= 0 ? guesses[latestGuessIndex] : null;
    const entryCount = guesses.length + hintHistory.length;
    let latestMarkup = '';

    if (latestGuess) {
        const latestClade = latestGuess.proximity.lastCommonClade
            ? `<div class="latest-guess-clade">${t('history.lastCommon', {
                clade: escapeHtml(latestGuess.proximity.lastCommonClade)
            })}</div>`
            : '';
        const explanationButton = currentGameMode === 'practice'
            && latestGuess.dino.nome !== targetDino?.nome
            ? `<button type="button" class="guess-explain-button latest-guess-explain" onclick="explainPracticeGuess(${latestGuessIndex})">${t('history.explain')}</button>`
            : '';

        latestMarkup = `
        <article class="latest-guess" aria-live="polite" aria-atomic="true">
            <div class="latest-guess-kicker">${t('history.latestGuess')}</div>
            <div class="latest-guess-main">
                <span class="latest-guess-name">${escapeHtml(latestGuess.dino.nome)}</span>
                <span class="latest-guess-match">${t('history.nodes', {
                    matches: latestGuess.proximity.matches,
                    depth: currentTargetDepth,
                    percent: latestGuess.proximity.percentage
                })}</span>
            </div>
            ${latestClade}
            ${explanationButton}
        </article>`;
    }

    let historyItems = '';

    if (hintHistory.length > 0) {
        hintHistory.slice().reverse().forEach(hint => {
            const isCladeHint = Boolean(hint.cladeName);
            const hintName = isCladeHint
                ? t('history.cladeHint', { clade: escapeHtml(hint.cladeName) })
                : t('history.nameHint');
            const hintDetail = isCladeHint
                ? t('history.revealedDepth', { depth: hint.depth })
                : escapeHtml(hint.message || t('game.nameHintFallback'));
            historyItems += `
            <div class="guess-item guess-item-hint">
                <span class="guess-name">${hintName}</span>
                <span class="guess-match">${hintDetail}</span>
            </div>
            `;
        });
    }

    guesses.slice().reverse().forEach((guess, reverseIndex) => {
        const guessIndex = guesses.length - reverseIndex - 1;
        const divInfo = guess.proximity.lastCommonClade
            ? ` → ${t('history.lastCommon', { clade: escapeHtml(guess.proximity.lastCommonClade) })}`
            : '';
        const explanationButton = currentGameMode === 'practice' && guess.dino.nome !== targetDino?.nome
            ? `<button type="button" class="guess-explain-button" onclick="explainPracticeGuess(${guessIndex})">${t('history.explain')}</button>`
            : '';

        historyItems += `
            <div class="guess-item">
                <span class="guess-name">${escapeHtml(guess.dino.nome)}${divInfo}</span>
                <span class="guess-match">${t('history.nodes', {
                    matches: guess.proximity.matches,
                    depth: currentTargetDepth,
                    percent: guess.proximity.percentage
                })}</span>
                ${explanationButton}
            </div>
        `;
    });

    historyDiv.innerHTML = `${latestMarkup}
        <details class="guess-history-details"${wasExpanded ? ' open' : ''}>
            <summary>
                <span class="guess-history-title">${t('history.title')}</span>
                <span class="guess-history-count">${t('history.entryCount', { count: entryCount })}</span>
            </summary>
            <div class="guess-history-list">${historyItems}</div>
        </details>`;
}