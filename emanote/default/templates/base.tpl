<!DOCTYPE HTML>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>
    <ema:titleFull />
  </title>
  <ema:metadata>
    <with var="page">
      <meta property="og:description" content="${value:description}" />
      <meta property="og:site_name" content="${value:siteTitle}" />
      <meta property="og:image" content="${value:image}" />
      <meta property="og:type" content="website" />
      <meta property="og:title" content="${ema:title}" />
    </with>
    <with var="template">
      <base href="${value:baseUrl}" />
      <link href="${value:iconUrl}" rel="icon" />
    </with>
    <snippet var="page.headHtml" />
  </ema:metadata>
  <tailwindCssShim />

  <style>
    /* Heist error element */
    strong.error {
      color: lightcoral;
      font-size: 90%;
      font-family: monospace;
    }

    /* External link icon */
    a[data-linkicon=""]::after {
      content: ""
    }

    a[data-linkicon=none]::after {
      content: ""
    }

    a[data-linkicon="external"]::after {
      content: url('data:image/svg+xml,\
      <svg xmlns="http://www.w3.org/2000/svg" height=".7em" viewBox="0 0 20 20"> \
        <path style="stroke:gray;stroke-width:1" d="M5 5v9M14 9v5M5 14h9M5 5h4M10 2h7M17 2v7M10 9l7-7"/> \
      </svg>');
    }

    a[data-linkicon="external"][href^="mailto:"]::after {
      content: url('data:image/svg+xml,\
        <svg \
          xmlns="http://www.w3.org/2000/svg" \
          height="0.7em" \
          fill="none" \
          viewBox="0 0 24 24" \
          stroke="gray" \
          stroke-width="2"> \
          <path \
            stroke-linecap="round" \
            stroke-linejoin="round" \
            d="M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z" /> \
        </svg>');
    }
  </style>
  <apply template="/templates/hooks/more-head" />

  <head-main />
  <apply template="components/stork/stork-search-head" />
</head>

<!-- DoNotFormat -->
<bind tag="theme"><ema:metadata><value var="template.theme" /></ema:metadata></bind>
<bind tag="iconSize">w-4 h-4 flex-shrink-0</bind>
<bind tag="bodyClass"><ema:metadata><value var="template.layout.base.bodyClass" /></ema:metadata></bind>
<!-- DoNotFormat -->

<body class="${bodyClass}">
  <body-main />
  <apply template="components/stork/stork-search" />
  <ema:metadata>
    <snippet var="page.bodyHtml" />
  </ema:metadata>
</body>

</html>