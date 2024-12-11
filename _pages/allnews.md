---
title: "News"
layout: textlay
sitemap: false
permalink: /allnews.html
---

# News

{% for article in site.data.news %}
<div style="overflow: hidden; margin-bottom: 20px;">
**{{ article.date }}** 
{{ article.headline | markdownify}}
</div>
{% endfor %}



<!--

{% for article in site.data.news %}
<p>{{ article.date }} <br>
<em>{{ article.headline | markdownify}}</em></p>
{% endfor %}

-->
