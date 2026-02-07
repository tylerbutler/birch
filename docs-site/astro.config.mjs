import fs from "node:fs";
import path from "node:path";

import netlify from "@astrojs/netlify";
import starlight from "@astrojs/starlight";
import { defineConfig } from "astro/config";
import starlightLinksValidator from "starlight-links-validator";

// Get the current script URL
const scriptUrl = new URL(import.meta.url);

const gleamGrammar = JSON.parse(
	fs.readFileSync(
		path.join(path.dirname(scriptUrl.pathname), "gleam.tmLanguage.json"),
		"utf-8",
	),
);

// https://astro.build/config
export default defineConfig({
	output: "static",
	adapter: netlify({
		imageCDN: false,
	}),
	site: "https://birch.tylerbutler.com",
	integrations: [
		starlight({
			title: "birch",
			description: "A logging library for Gleam with cross-platform support",
			tagline: "logs that gleam",
			lastUpdated: true,
			customCss: [
				"@fontsource/open-sans/400.css",
				"@fontsource/open-sans/600.css",
				"@fontsource-variable/fira-code",
				"./src/styles/custom.css",
			],
			plugins: [starlightLinksValidator()],
			expressiveCode: {
				shiki: {
					langs: [gleamGrammar],
				},
				styleOverrides: {
					codeFontFamily:
						"'Fira Code Variable', ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace",
				},
			},
			social: [
				{
					icon: "github",
					label: "GitHub",
					href: "https://github.com/tylerbutler/birch",
				},
			],
			sidebar: [
				{ slug: "index" },
				{ slug: "getting-started" },
				{ slug: "handlers" },
				{
					label: "API Reference",
					link: "https://hexdocs.pm/birch/",
					attrs: { target: "_blank" },
				},
			],
		}),
	],
});
