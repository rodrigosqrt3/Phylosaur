// ═══════════════════════════════════════════════
// AUTOCOMPLETE
// ═══════════════════════════════════════════════
function initializeAutocomplete() {
  const input = document.getElementById('dino-input');
  const sugDiv = document.getElementById('suggestions');
  
  if (!input || !sugDiv) return;

  input.setAttribute('role', 'combobox');
  input.setAttribute('aria-autocomplete', 'list');
  input.setAttribute('aria-controls', 'suggestions');
  input.setAttribute('aria-expanded', 'false');
  sugDiv.setAttribute('role', 'listbox');
  sugDiv.setAttribute('aria-label', 'Dinosaur suggestions');

  let status = sugDiv.parentElement?.querySelector('.autocomplete-status');
  if (!status) {
    status = document.createElement('div');
    status.className = 'visually-hidden autocomplete-status';
    status.setAttribute('aria-live', 'polite');
    status.setAttribute('aria-atomic', 'true');
    sugDiv.insertAdjacentElement('afterend', status);
  }

  input.addEventListener('dblclick', () => {
    if (!input.disabled) input.select();
  });

  const hideSuggestions = () => {
    sugDiv.style.display = 'none';
    input.setAttribute('aria-expanded', 'false');
    input.removeAttribute('aria-activedescendant');
    sugDiv.querySelector('.suggestion-item.highlighted')?.classList.remove('highlighted');
  };

  const highlightSuggestion = item => {
    sugDiv.querySelectorAll('.suggestion-item').forEach(suggestion => {
      suggestion.classList.toggle('highlighted', suggestion === item);
      suggestion.setAttribute('aria-selected', suggestion === item ? 'true' : 'false');
    });
    input.setAttribute('aria-activedescendant', item.id);
    input.value = item.textContent.trim();
  };

  input.addEventListener('input', function() {
    const text = this.value.toLowerCase().trim();
    
    if (text.length < 2) {
      status.textContent = '';
      hideSuggestions();
      return;
    }
    
    const matches = database
      .filter(d => 
        d.nome.toLowerCase().includes(text) && 
        !guessedNames.has(d.nome.toLowerCase())
      )
      .slice(0, 8);
    
    if (!matches.length) {
      status.textContent = 'No dinosaur suggestions available.';
      hideSuggestions();
      return;
    }
    
    sugDiv.innerHTML = matches
      .map((d, index) => `<div class="suggestion-item" id="dino-suggestion-${index}" role="option" aria-selected="false" onclick="selectSuggestion('${d.nome}')">${d.nome}</div>`)
      .join('');
    
    sugDiv.style.display = 'block';
    input.setAttribute('aria-expanded', 'true');
    status.textContent = `${matches.length} dinosaur suggestion${matches.length === 1 ? '' : 's'} available.`;
  });

  input.addEventListener('keydown', e => {
    const items = sugDiv.querySelectorAll('.suggestion-item');
    const current = sugDiv.querySelector('.suggestion-item.highlighted');
    const currentIndex = Array.from(items).indexOf(current);

    if (e.key === 'ArrowDown') {
      e.preventDefault();
      if (sugDiv.style.display === 'none' || items.length === 0) return;
      const next = items[currentIndex + 1] || items[0];
      highlightSuggestion(next);
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      if (sugDiv.style.display === 'none' || items.length === 0) return;
      const prev = items[currentIndex - 1] || items[items.length - 1];
      highlightSuggestion(prev);
    } else if (e.key === 'Enter') {
      e.preventDefault();
      if (document.querySelector('.modal-overlay')) return;

      if (current) {
        input.value = current.textContent.trim();
        hideSuggestions();
      }
      makeGuess();
    } else if (e.key === 'Tab') {
      hideSuggestions();
    } else if (e.key === 'Escape') {
      hideSuggestions();
    }
  });

  if (window.phylosaurAutocompleteCleanup) {
    window.phylosaurAutocompleteCleanup();
  }

  const outsideClickHandler = e => {
    if (e.target !== input && !sugDiv.contains(e.target)) hideSuggestions();
  };
  document.addEventListener('click', outsideClickHandler);
  window.phylosaurAutocompleteCleanup = () => {
    document.removeEventListener('click', outsideClickHandler);
  };
}

function selectSuggestion(name) {
  const input = document.getElementById('dino-input');
  const sugDiv = document.getElementById('suggestions');
  
  if (input && sugDiv) {
    input.value = name;
    sugDiv.style.display = 'none';
    input.setAttribute('aria-expanded', 'false');
    input.removeAttribute('aria-activedescendant');
    input.focus();
  }
}