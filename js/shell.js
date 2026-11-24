// ═══════════════════════════════════════════════
// SHARED SHELL: theme switch and glass select
// Loaded by both the game and the About page so the header controls behave identically.
// Needs state.js (currentTheme, PHYLOSAUR_STORAGE_KEYS) and i18n.js (t).
// ═══════════════════════════════════════════════
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
    // Measure the button's own layout box, not its drawn rectangle: while the mouse button is
    // held the trigger is scaled to 95% (":active"), and the click that opens the menu fires
    // before it grows back. Measuring the scaled rectangle made the menu 5% narrower and 5px
    // off to the side whenever the click lasted more than a few milliseconds.
    const drawn = state.trigger.getBoundingClientRect();
    const layoutWidth = state.trigger.offsetWidth || drawn.width;
    const layoutHeight = state.trigger.offsetHeight || drawn.height;
    const centerX = drawn.left + drawn.width / 2;
    const centerY = drawn.top + drawn.height / 2;
    const rect = {
        left: centerX - layoutWidth / 2,
        right: centerX + layoutWidth / 2,
        top: centerY - layoutHeight / 2,
        bottom: centerY + layoutHeight / 2,
        width: layoutWidth,
        height: layoutHeight,
    };
    // The menu is exactly as wide as the button that opened it. Only a small icon-like
    // button (the language picker, ~40px) gets a wider menu, because 40px cannot hold names.
    const width = rect.width >= 120
        ? Math.min(rect.width, window.innerWidth - 16)
        : Math.min(Math.max(rect.width, 190), window.innerWidth - 16);
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

// Slides the underline of a tab row from the tab that was active to the one that now is: the
// leading edge moves first and the trailing edge follows, so it stretches and settles (the same
// motion as the header highlight). Skipped when the system asks for reduced motion.
const tabGlideFrames = new WeakMap();

function glideTabMarker(row, fromTab, toTab) {
    if (!row || !fromTab || !toTab || fromTab === toTab || typeof requestAnimationFrame !== 'function') return;
    if (globalThis.matchMedia?.('(prefers-reduced-motion: reduce)').matches) return;
    cancelAnimationFrame(tabGlideFrames.get(row));
    row.querySelector(':scope > .tab-glide')?.remove();
    const marker = document.createElement('span');
    marker.className = 'tab-glide';
    marker.setAttribute('aria-hidden', 'true');
    row.append(marker);
    row.setAttribute('data-gliding', '');

    const origin = row.getBoundingClientRect();
    const edgeLeft = origin.left + (row.clientLeft || 0);
    const boxOf = tab => {
        const box = tab.getBoundingClientRect();
        return { left: box.left - edgeLeft, right: box.right - edgeLeft };
    };
    const start = boxOf(fromTab);
    const end = boxOf(toTab);
    const rightward = end.left >= start.left;
    const mix = (a, b, amount) => a + (b - a) * amount;
    const lead = p => 1 - Math.pow(1 - p, 3);
    const trail = p => p * p * (3 - 2 * p);
    const started = performance.now();
    const duration = 460;

    const frame = now => {
        if (!marker.isConnected) return;
        const p = Math.min(1, (now - started) / duration);
        const left = mix(start.left, end.left, rightward ? trail(p) : lead(p));
        const right = mix(start.right, end.right, rightward ? lead(p) : trail(p));
        marker.style.left = `${left}px`;
        marker.style.width = `${Math.max(0, right - left)}px`;
        if (p < 1) {
            tabGlideFrames.set(row, requestAnimationFrame(frame));
        } else {
            marker.remove();
            row.removeAttribute('data-gliding');
        }
    };
    tabGlideFrames.set(row, requestAnimationFrame(frame));
}