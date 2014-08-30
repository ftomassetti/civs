(ns
  ^{:author ftomassetti}
  civs.model.language)

(defn generate-language []
  (com.github.langgen.SamplesBasedLanguageFactory/getRandomLanguage))
