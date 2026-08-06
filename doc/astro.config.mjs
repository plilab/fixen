// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
import remarkMath from "remark-math";
import rehypeMathjax from "rehype-mathjax";
import remarkDirective from "remark-directive";
import remarkDirectiveRehype from "remark-directive-rehype";
import rehypeComponents from "rehype-components";
import { h } from "hastscript";
import { directives } from "./src/directives.ts";
import rehypeNumbered from "./src/rehype-numbered.js";

// https://astro.build/config
export default defineConfig({
  site: "https://fixen-lang.org",
  markdown: {
    remarkPlugins: [remarkMath, remarkDirective, remarkDirectiveRehype],
    rehypePlugins: [
      [
        rehypeNumbered,
        {
          refName: {
            eg: "Example",
            def: "Definition",
            thm: "Theorem",
            prop: "Proposition",
            conj: "Conjecture",
            lem: "Lemma",
            cor: "Corollary",
            noneg: "Nonexample",
          },
        },
      ],
      rehypeMathjax,
      [
        rehypeComponents,
        {
          components: directives,
        },
      ],
    ],
  },
  integrations: [
    starlight({
      customCss: [
        "./src/global.css",
        "@fontsource-variable/geist/index.css",
        "@fontsource-variable/geist-mono/index.css",
      ],
      title: "Fixen",
      social: [
        {
          icon: "github",
          label: "GitHub",
          href: "https://github.com/plilab/fixen",
        },
      ],
      components: {
        Sidebar: "./src/components/starlight/Sidebar.astro",
        PageTitle: "./src/components/starlight/PageTitle.astro",
      },
      sidebar: [
        {
          label: "Guides",
          items: [{ autogenerate: { directory: "guides" } }],
        },
      ],
    }),
  ],
});
