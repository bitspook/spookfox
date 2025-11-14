window.translate = (input) => {
  sourceEl = document.querySelector('[aria-labelledby="translation-source-heading"] d-textarea')

  if (!sourceEl) return null

  sourceEl.value = input
  sourceEl.dispatchEvent(new InputEvent('change'))

  targetEl = document.querySelector('[aria-labelledby="translation-target-heading"] d-textarea')

  if (!targetEl) return null

  return new Promise(resolve => {
    setTimeout(() => resolve(targetEl.value), 500)
  })
}
