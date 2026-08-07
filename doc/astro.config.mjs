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
import starlightVersions from "starlight-versions";

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
      logo: {
        light: "./src/assets/fixen-logo-only-black.svg",
        dark: "./src/assets/fixen-logo-only-white.svg",
        replacesTitle: true,
      },
      plugins: [
        starlightVersions({
          versions: [{ slug: "0.0.0.1" }],
        }),
      ],
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
          items: [
            {
              label: "Getting Started",
              items: [
                { autogenerate: { directory: "guides/getting-started" } },
              ],
            },
            "guides/what-is-fpop",
            {
              label: "Your First Program",
              items: [
                { autogenerate: { directory: "guides/your-first-program" } },
              ],
            },
            {
              label: "Language Essentials",
              items: [
                { autogenerate: { directory: "guides/language-essentials" } },
              ],
            },
            {
              label: "Graph Algorithms",
              items: [
                { autogenerate: { directory: "guides/graph-algorithms" } },
              ],
            },
            {
              label: "Static Analysis",
              items: [
                { autogenerate: { directory: "guides/static-analysis" } },
              ],
            },
            {
              label: "Haskell Interop",
              items: [
                { autogenerate: { directory: "guides/haskell-interop" } },
              ],
            },
            {
              label: "Understanding Generated Code",
              items: [
                {
                  autogenerate: {
                    directory: "guides/understanding-generated-code",
                  },
                },
              ],
            },
          ],
        },
        {
          label: "Language Reference",
          items: [{ autogenerate: { directory: "reference" } }],
        },
        {
          label: "Advanced Topics",
          items: [{ autogenerate: { directory: "advanced" } }],
        },
        {
          label: "Publications",
          items: [{ autogenerate: { directory: "publications" } }],
        },
        {
          label: "Community",
          items: [{ autogenerate: { directory: "community" } }],
        },
      ],
    }),
  ],
});
