// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): block.with(
    fill: luma(230), 
    width: 100%, 
    inset: 8pt, 
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != "string" {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    new_title_block +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: white, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: (),
  fontsize: 11pt,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: "1",
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)

  if title != none {
    align(center)[#block(inset: 2em)[
      #text(weight: "bold", size: 1.5em)[#title]
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)
#show: doc => article(
  title: [Implications of Measurement Errors in Water Use Estimates for Water Management],
  authors: (
    ( name: [Taro Mieno#footnote[University of Nebraska-Lincoln, Email: tmieno2\@unl.edu];],
      affiliation: [],
      email: [] ),
    ( name: [Timothy Foster#footnote[University of Manchester];],
      affiliation: [],
      email: [] ),
    ( name: [Nicholas Brozovic#footnote[University of Nebraska-Lincoln];],
      affiliation: [],
      email: [] ),
    ),
  sectionnumbering: "1.1.a",
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)


= Main
<main>
== Background
<background>
- Use of satellite imagery in water management in general

  - water theft detection in Australia (Nature Water, Adam Loch)

- intensity estimation accuracy

- (estimated) intensity-based vs (estimated) area-based (Southern Europe History)

- (estimated) intensity-based vs (true) intensity-based \[We work on this\]

- Lack of measurement. Use of water use estimates for water management (current and future). Emergence of Open ET as a substitute.

key objective:

- start a discussion about the implications of the use of mis-measured water use for water use regulation.

- shed lights on unintentional consequences of using estimates rather than actual measurements

Two policies:

- water quota (examples of existing quota systems)
- water permit trading

No clear policy design suggestions on how to use water use estimates with measurement errors.

= Results
<results>
== Profit under water and proxy quota
<profit-under-water-and-proxy-quota>
@fig-profit-quota-water-het illustrates the profit from agricultural production as a function of actual water use, comparing water quotas and proxy quotas across three levels of production heterogeneity and varying sizes of measurement errors. When there is no production heterogeneity among farmers, the water quota is the most cost-effective mechanism to achieve a targeted reduction in water use (yielding the highest profit for a given level of actual water use). In this scenario, the proxy quota with unbiased measurement errors performs worse than the water quota due to the asymmetry in downside and upside risk of water use. However, as production heterogeneity increases, the comparative advantage of water quotas over proxy quotas diminishes.

It is demonstrated (proof in Appendix) that water quotas consistently outperform proxy quotas on average, regardless of production heterogeneity, when measurement errors are independent and the production function is concave. As expected, the profit losses associated with proxy quotas increase as the size of measurement errors grows. Furthermore, the profit loss from using proxy-based quotas tends to become more pronounced as total realized water use decreases.

#figure([
#box(image("figures/g_profit_quota_comp_stat.png"))
], caption: figure.caption(
position: bottom, 
[
The impacts of an increase in the degree of measurement error on profit
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-profit-quota-water-het>


#cite(<grafton2018paradox>, form: "prose")

We now look at simple two-farm examles to build intuition behind the profit loss due to the use of proxy quota in place of water quota. @fig-illust-homegeneous presents a case with two homegeneous farmers under water and proxy quota. The red dotted lines indicate the effective water quota. Under the water quota system, both farmers receive 7 inches of water quota. Since the unconstrained profit maximizing water use is 10.94 inches, both farmers use up all the allocated quota and use 10 inches of water. Under the proxy quota system, they are allocated the same amount of proxy quota. However, measurement error in proxy quota resulted in effective water quota of 4 and 10 inches for farmer A and B, respectively. Both farmers use their allocated amout of water. Under both proxy and water quota systems, the total water use is 14 inches. Farmer B makes 29.9 less under the water quota system compared to the proxy quota system. However, Farmer A makes 128.9 more under the water quota system compared to the proxy quota system. This comes from an important nature of crop production with respsect to water: declining marginal impact of water on crop yield. As can be seen in the profit-water relationship, removing an inch of water from 6 inches has much larger negative impact compared to revmoing an inch of water from 10 inches.

#figure([
#box(image("figures/g_illustration_homogeneous.png"))
], caption: figure.caption(
position: bottom, 
[
Illustration of profit loss due to the use of proxy for quota-based regulation: a case of homegeneous producers
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-illust-homegeneous>


However, as the heterogeneity of farmers increases, comparative advantage of water quota over proxy quota diminishes. While the performance of both systems declines as the level of heterogeneity increases, water quota is affected more so than the proxy quota system. This is because uniform quota now acts like heterogeneous quota under homogeneous production function because the same quota is given to farmers with heterogeneous water demand. This is illustrated in @fig-illust-heterogeneous. In this example, farmer A needs less water than farmer B. Water and proxy quota allocated to the farmers are exactly the same as the previous case. In this example, errors in proxy happened to realize so that more effective water quota is given to farmer B than farmer A. Consequently, proxy quota produces a higher total profit than water quota. Of course, if error happened to go the opposite way, then the performance of proxy quota relative to water quota is even worse compared to the homogeneous case. This is a big contrast to the homogeneous case because water quota is always better than proxy quota in that case. Consequently, proxy quota can perform relatively better under heterogeneous cases. On average, the positive and negative effects of measurement errors tend to even out. However, proxy quota is never better than water quota #strong[on average] (see Appendix … for mathematical proof).

#figure([
#box(image("figures/g_illustration_het.png"))
], caption: figure.caption(
position: bottom, 
[
Illustration of profit loss due to the use of proxy for quota-based regulation: a case of heterogeneous producers
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-illust-heterogeneous>

== The effect of allowing trading quota
<the-effect-of-allowing-trading-quota>
Allowing quota to be traded among farmers can dramatically change the relative performance of the water- and quota-based regulation. @fig-trading-profit presents profit for water-based quota, proxy-based quota, water-based quota trading, and proxy-based quota trading regulation systems. As shown in the figure, the disadvantage of using proxy almost disappears when trading of quota is allowed. This is because the inefficiencies in quota allocations are resolved through trading of quota whether it is water or proxy quota.

#figure([
#box(image("figures/g_trade_effect_profit.png"))
], caption: figure.caption(
position: bottom, 
[
Comparison of profit from agricultural production between the water and quota systems when quota trading is allowed
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-trading-profit>


@fig-trading-profit-illustration illustrates the impact of allowing for trading proxy quota using the case of two homogeneous farmers illustrated in @fig-illust-homegeneous. In this illustration, the same water quota of 7 is allocated for both farmers under the water-based systems. No water quota trading would happen as the marginal profit of water is already equated at water use of 7. Under the proxy-base quota system, proxy quota of 7 is allocated for both farmers, but the effective water quota for farmers A and B are 4 and 7, respectively. Once trading of proxy quota is alllowed, farmer A would buy 1.85 proxy quota from farmer B. At these production levels, the marginal profit of quota (not water) are equated acros the farmers and no further transfer between them would happen. Trading of proxy quota improves the overall profit to 920.08 \$/acre from 831.94 \$/acre.

#figure([
#box(image("figures/g_illustration_homogeneous_with_trade.png"))
], caption: figure.caption(
position: bottom, 
[
Illustration of the benefits of trading quota
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-trading-profit-illustration>


While water-quota and proxy-quota trading perform about the same, water-quota always leads to a higher total profit for a given level of realized water use. In other words, to achieve a certain water use goal, water-quota trading is always to more cost-effective than proxy-quota trading. Under water-quota trading, the margial profit of water is equalized across the farmers, which is a necessary condition to achieve the highest profit for a given level of total water use. Under proxy-quota trading, the marginal profit of proxy is equalized across the farmers, not the margial profit of water (See Appendix … for a mathematical proof). However, it is worth emphasizing that the inefficiency caused by the use of proxy is likely to be minimal as shown in @fig-trading-profit. This point is well-illustrated in @fig-trading-profit-illustration, where total profit per acre is 930.91 and 920.08 under the water-quota and proxy-quota trading system, respsectively.

@fig-trading-profit presents the simualtion results of proxy-quota and water-quota trading systems added to @fig-profit-quota-water-het. As you can see at all levels of production heterogeneity levels and the degres of measurement errors, proxy-quota and water-quota trading systems perform almost identical at all levels of realized water use (water-quota trading is always slightly better than proxy-quota trading as explained earlier). Not surprisingly, the benefit of allowing for trading is greater when the degree of measurement errors is larger.

== Use of proxy can exercebate distributional inequity
<use-of-proxy-can-exercebate-distributional-inequity>
@fig-distributional-equity presents the cumulative probability of the ratio of profit under quota-based relative to water-based system for quota (red) and permit trading (blue). Ratio of less than 1 indicates making less profit under the quota-based system compared to the water-based system. If farmers make just as much profir under proxy-based regulation compared to water-based regulation, then the cumulative probability line would be a vertical line at 1. For example, under the high degree of measurement error and medium heterogenenity scenario, some farmers would make about the half of what they would make under the water quota system. At the lower end of the distribution are farmers who would be constrained severely by water quota, but whose water use estiamtes are much higher than their actual water use. At the higher end of the distribution are farmers who would be constrained by water quota, but whose water use estiamtes are much lower than their actual water use. Famres who suffer significantly under the quota system can develop strong dissatisfaction of the quota-based regulation system, which in turn may lead to installing water meter themselves. If they discover the over-estimation of their water use, they may appeal to legal actions against the administrator for their unfair treatment. Allowind quota trading greatly diminishes the distributional inequality, though not completely. This is because unfortunate farmers who suffered from over-estimation of their water use can now at least purchase proxy quota to increase their production level.

#figure([
#box(image("figures/g_profit_ratio.png"))
], caption: figure.caption(
position: bottom, 
[
Distribution of profit under various regulation systems
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-distributional-equity>


= Discussion
<discussion>
- Unbiased proxy
  - can hurt profit \
  - elements that matter
    - the heterogeneity of production function
    - the degree of measurement error
- Allowing quota trading can be particularly beneficial when proxy is used.
  - Trade-off of implementing quota trading (this is not anything new)
    - gain in profit from agricultural production
    - administrative cost
- Use of proxy exercebates distributional inequality
  - trading quota can help here again
  - greater chance of litigations from farmers against the administrator
- Limitations
  - Assumptions (farmers have linked their practice and water use estimates, but not water)
    - uncertainty in water use estimates lead to under-use of water, harming profit even more
  - Temporal
    - one-year decision
    - other quota designs (multiple year)

= Methods
<methods>
This section describes the details of the simulation analysis conducted in this article. Our simulation imagines a region with 1000 (denoted as $N$, hereafter) farmers, where a single water governing body possesses a right to devise groundwater policies. Our simulation considers nine scenarios in total: three different levels of production heterogeneity times three different degrees of measurement errors. Under each scenario, 1000 rounds of individual simulations are run. In each simulation round, measurement errors are drawn randomly for the individual farmers, and then average (across the farmers) water use and profit are calculated at different levels of water or proxy quota levels for four different policies: water-quota, proxy-quota, water-quota trading, proxy-quota trading. The summary of the 1000 simulation rounds. The key components of the simluation analysis are presented below.

== Crop-water production function
<crop-water-production-function>
For all faemers $i in 1 , dots.h , N$, the crop-water production function follows the Michelich-Baule functional form presented below(citations).

where $y$ is corn yield (kg/ha), $w$ is water use (mm), and $phi.alt$, $alpha$, and $beta$ are model parameters that govern how corn yield responds to water use.

Given the price of corn ($P_c$) and water ($P_w$), farmer $i$ is assumed to maximize profit by solving the following maximization problem when no water quota is placed:

Three different sets of the collections of crop-water production functions are generated that vary in the degree of heterogeneity in unconstrained optimal water use ($w^(\*)$). Under no-heterogeneity set, farmers share exactly the same profuction function (the same value of $phi.alt_i$, $alpha_i$, and $beta_i$). Unde the medium-heterogeneity set, model parameters vary and so does $w^(\*)$. Under the high-heterogeneity set, the variance of $w^(\*)$ becomes even greater. The distribution of $w^(\*)$ for each of the sets is presented in @fig-dist-opt-w.

== Measurement error in water use
<measurement-error-in-water-use>
When water use values are not measured and observed, the administrator estimates water use. The estimated value of water use is named "proxy" and denoted as $p$. Proxy and water are assumed to follow the relationship below for all the farmers ($i in 1 , dots.h , N$):

where $eta_i$ is a scalar greater than 0. Measurement error (denoted as $mu$) can be written as follows:

While farmers with $eta_i > 1$ are unfortunate and their water uses are over-estimated, farmers with $eta_i < 1$ are fortunate and their water uses are under-estimated.

Three different sets of the distribution of $eta$ are considered that vary in the degree of heterogeneity in $eta$: low, medium, and high. For all the sets, $E [eta] = 1$ (or equivalently $E [mu] = 0$), meaning that water use estimation is ubiased. A sample of the empirical (realized) distribution of $eta$ for each of the sets is presented in #strong[?\@fig-dist-eta];.

== Water use under quota and trading systems
<water-use-under-quota-and-trading-systems>
Under both water and proxy quota systems without trading, the same amount of quota is allocated to all the farmers. (this is realistic with insufficient information available to the administrator). For a given water quota of $overline(w)$, farmer $i$’s water use would be $m i n { w^(\*) , overline(w) }$. Under the proxy quota system, the same amount of proxy quota ($overline(p)$) is allocated to all the farmers. However, the effective limit on water use varies by farmer. Given the proxy quota of $overline(p)$, the effective water use limit for farmer $i$ is $overline(p) \/ eta_i$, and uses $m i n { w^(\*) , overline(p) \/ eta_i }$.

Assumptions: + farmers: has learned the degree of under- or over-estimation of water use compared to actual over the years and has learned how to irrigate up to the water quota

Under a water trading system, the same amount of water quota is first allocated to all the farmers just like water quota system. However, farmers are now allowed to sell or buy water quota under a water trading system. Assuming no transaction cost of trading, water quota will be traded until the marginal profit of water at their respective level of water uses is equated across the farmers. Let $pi_i (w)$ denote farmer $i$’s profit: $P_c dot.op f_i (w) - P_w dot.op w$. Conditional on water quota of $overline(w)$, individual water uses for the farmers can be identified by finding a series of water use values ($w_1 , w_2 , dots.h , w_N$) that satisfy the following conditions:

Under a proxy trading system, proxy quota will be traded until the marginal profit of proxy (not water) at their respective level of effective water (or equivalently proxy) uses is equated across the farmers. Conditional on proxy quota of $overline(p)$, individual proxy use for the farmers can be identified by finding a series of proxy use values ($p_1 , p_2 , dots.h , p_N$) that satisfy the following conditions:

== Data and Code Availability
<data-and-code-availability>
The datasets created in this article are all computer-generated using R (citation). All the codes and datasets are available at #link("https://github.com/tmieno2/Measurement-Error-Water-Management")[this GitHub repository];. Instructions for reproducing all the outputs presented in this article are provided at the bottom of the page.

#block[
#heading(
level: 
1
, 
numbering: 
none
, 
[
(APPENDIX) Appendix
]
)
]
== Distribution of optimal water use under no water use constraints
<distribution-of-optimal-water-use-under-no-water-use-constraints>
#figure([
#box(image("figures/g_opt_w_dist.png"))
], caption: figure.caption(
position: bottom, 
[
Distribution of optimal water use under no water use constraints
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-dist-opt-w>


== Distribution of water-proxy conversion factor
<distribution-of-water-proxy-conversion-factor>



#set bibliography(style: "apa")

#bibliography("DRA.bib")

