#lang scribble/manual

@(require
  (for-label
   racket/base
   racket/contract
   llm/cost-base))

@title{LLM Cost Model}
@defmodule[llm/cost-base]

@racketmodname[llm] lang integrates several cost models of LLMs to report
training and inference costs after each query.
This feedback is reported after each prompt is sent to the
@racket[current-cost-port], which is set to @racket[current-error-port] by
default.
The intention is to ensure the developer is given feedback about the potential
resource cost of their query to encourage better prompt-engineering practice.

@bibliography[
(bib-entry
 #:key "faiz2023"
 #:author "Faiz, Ahmad and Kaneda, Sotaro and Wang, Ruhan and Osi, Rita and Sharma, Prateek and Chen, Fan and Jiang, Lei"
 #:title "LLMCarbon: Modeling the end-to-end Carbon Footprint of Large Language Models"
 #:url "https://doi.org/10.48550/ARXIV.2309.14393"
 #:location "ICLR"
 #:date "2023")
(bib-entry
 #:key "wilkins2024a"
 #:title "Offline Energy-Optimal LLM Serving: Workload-Based Energy Models for LLM Inference on Heterogeneous Systems"
 #:author "Wilkins, Grant and Keshav, Srinivasan and Mortier, Richard"
 #:url "https://doi.org/10.48550/ARXIV.2407.04014"
 #:date "2024")
(bib-entry
 #:key "wilkins2024"
 #:title "Online Workload Allocation and Energy Optimization in Large Language Model Inference Systems"
 #:author "Wilkins, Grant"
 #:url "https://grantwilkins.github.io/gfw27_project.pdf"
 #:note "MSc. Thesis"
 #:date "2024")
(bib-entry
 #:key "li2023"
 #:title "Making AI Less \"Thirsty\": Uncovering and Addressing the Secret Water Footprint of AI Models"
 #:author "Li, Pengfei and Yang, Jianyi and Islam, Mohammad A. and Ren, Shaolei"
 #:url "https://doi.org/10.48550/ARXIV.2304.03271"
 #:date "2023")
]

The training costs are based on the work of @cite{faiz2023}.
A copy of the tooling, with additional data and estimates used to generate the
training costs are included in the repository for @racketmodname[llm] lang.

The inference models are based on the work of @cite{wilkins2024} and @cite{wilkins2024a}.
These estimates are even more rough, but unfortunately the best I can do with non-local LLMs.

@defstruct*[inference-cost-info ([input-tokens natural-number/c]
                                 [output-tokens natural-number/c]
                                 [prompt-duration natural-number/c]
                                 [response-duration natural-number/c])]{
Information related to the prompt and response of a single inference query,
given as input to a @racket[gen:kwh-model] to compute the power cost of the
query.
}

@defidform[gen:kwh-model]{
A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{generic
interface} that defines a model of the KWh consumption of an LLM inference.
Any such model defines a method @racket[model->kwh].
}

@defproc[(model->kwh [model gen:kwh-model] [info inference-cost-info?]) natural-number/c]{
Returns the KWh consumption of the query specified by @racket[info], according to the model @racket[model].
}

@defstruct*[wilkins-inference-model ([alpha-k-s-0 number?] [alpha-k-s-1 number?] [alpha-k-s-2 number?])]{
A @racket[gen:kwh-model] specifying the consumption according to Wilkins' model @cite["wilkins2024" "wilkins2024a"].
This model is parameterized by a system @emph{S}, and 3 magic numbers computed experimentally.
Wilkins gives a table of computed numbers for some common LLMs and 1 system, the AMD+A100.
The magic numbers are highly correlated with the parameter count of the model,
and we use that the guestimate the values for various backends.
}

@defstruct*[time-avg-inference-model ([kw natural-number/c])]{
A @racket[gen:kwh-model] specifying the consumption as a function of the
wattage of the system and the duration of the query.
This is more accurate if you know the wattage of the system, such as when
running locally, but difficult to determine for a proprietary hetergeneous
system in some data center.
}

@defstruct*[model-cost-info ([model-name (or/c string? symbol?)]
                             [query-tco2/kwh natural-number/c]
                             [query-water-L/kWh natural-number/c]
                             [training-water-L/kWh natural-number/c]
                             [training-tco2 natural-number/c]
                             [training-kwh natural-number/c]
                             [inference-model gen:kwh-model])]{
The cost information for a particular LLM.

@racket[model-name] is used for debugging and reporting, and represents the model's name to a developer.

@racket[query-tco2/kwh] represent the metric tons of CO2 used to produce a KWh
during an inference query.
This information is specific the power generation for the power used to the
language model during inference.

@racket[query-water-L/kWh] represents the litres of water used per KWh during an inference query.
This is the on-site scope 1 usage, which you can think of as the water
consumptions of cooling aparatus in a data centre.
It's probably 0 for a local LLM.
See @cite{li2023} for more info.

@racket[training-water-L/kWh] represents the litres of water used per KWh
during training of the LLM.
This is meant to be the on-site scope 1 usage.

@racket[training-tco2] represents the metric tons of CO2-equivalent emitted during training.
This captures the power generation and emboddied carbon of the GPUs.

@racket[training-kwh] represents the kilowatt-hours used to power computation while training the LLM.

@racket[inference-model] represents the @racket[gen:kwh-model] used for inferences to this LLM.
}

@defparam[current-model-cost-log log (list/c cost-log-entry?) #:value '()]{
A list of @racket[cost-log-entry]s for the current LLM session.
}

@defstruct*[cost-log-entry ([model-cost-info model-cost-info?] [inference-info inference-cost-info?])]{
An entry in the inference cost log @racket[current-model-cost-log], recording
the cost model information for the LLM backend, and the cost information for a
given query.
}

@defparam[current-cost-port port output-port? #:value (current-error-port)]{
The port to which the @racket[current-model-cost-log] is written, using
@racket[current-model-cost-logger], after each call to @racket[prompt!].
}

@defparam[current-model-cost-logger logger (-> (list/c cost-log-entry?) (list/c cost-log-entry?))
          #:value string-stderr-model-cost-logger]{
A parameter defining how to write the log after each call to @racket[prompt!].
The @racket[logger] should return a (possibly modified) log, in order to be able to compose loggers.
}

@defproc[(string-stderr-model-cost-logger [log (list/c cost-log-entry?)]) (list/c cost-log-entry?)]{
A logger that writes the log, as a string, the @racket[current-cost-port],
which is expected to be the standard error port, returning the log unchanged.
Uses @racket[log->string] to produce a readable string represetation of the log.
}

@defproc[(log->string [log (list/c cost-log-entry?)]) string?]{
Render the log as a readable string representation, with information contextualizing the resource cost of the log.
}

@defproc[(log-model-cost! [entry cost-log-entry?]) void?]{
Prepend @racket[entry] to the @racket[current-cost-log].
}
