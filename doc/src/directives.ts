
import {h} from 'hastscript';

const Theorem = (properties, children) => {
  if (properties.title) {
    return h("div.thm.not-content", { id : properties.id },
      h("div.thm-header", 
        h("strong", properties.name),
        " ("+properties.title +")."
      ),
      ...children
    )
  } else {
    return h("div.thm.not-content", { id : properties.id },
      h("div.thm-header", 
        h("strong", properties.name),
        "."
      ),
      ...children
    )

  }
}

const Proof = (properties, children) => {
  return h("div.proof",
      h("div.thm-header", 
        h("strong", "Proof.")),
         h("div.qed"),
         ...children)
}

const Steps = (properties, children) => {
  if (children.length !== 1 || children[0].tagName !== "ol") {
    return h("div", "Invalid steps")
  }
  let rootElement = children[0];
  rootElement.properties.role = 'list';
  // Add the required CSS class name, preserving existing classes if present.
  if (!Array.isArray(rootElement.properties.className)) {
    rootElement.properties.className = ['sl-steps'];
  } else {
    rootElement.properties.className.push('sl-steps');
  }

  // Add the `start` attribute as a CSS custom property so we can use it as the starting index
  // of the steps custom counter.
  if (typeof rootElement.properties.start === 'number') {
    const styles = [`--sl-steps-start: ${rootElement.properties.start - 1}`];
    if (rootElement.properties.style) styles.push(String(rootElement.properties.style));
    rootElement.properties.style = styles.join(';');
  }
  return rootElement;
}


export const directives = {
  'theorem': Theorem,
  'proof': Proof,
  'steps': Steps
}
