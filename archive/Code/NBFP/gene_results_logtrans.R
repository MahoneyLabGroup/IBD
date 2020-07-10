gene_results_formatr <- function(file = NULL, 
                                 gprof_org = c("hsapiens", "mmusculus"), 
                                 gprof_target = c("ENTREZGENE"), 
                                 gprof_num_ns = c("ENTREZGENE_ACC")) {
  tissue_table <- read_csv(file) %>% 
  select(GENE, P, Mean.FP.Rate) %>% 
  mutate(
    log_p = -log10(P),
    log_fpr = -log10(Mean.FP.Rate) 
  ) %>% 
  rowwise() %>%
  mutate(comb_score = sum(log_p > .$log_p & log_fpr > .$log_fpr)) %>%
  ungroup() %>%
  mutate(comb_score = comb_score/n())

## Do gene conversion at the end
genes <- gprofiler2::gconvert(as.character(t(tissue_table$GENE)),
                              organism = gprof_org,
                              target = gprof_target,
                              numeric_ns = gprof_num_ns) %>% 
  filter(!duplicated(name), !duplicated(input)) %>% 
  as_tibble() %>% 
  rename(GENE = input) %>% 
  select(GENE, name) %>% 
  mutate(across(GENE, as.numeric))

tissue_table_genes <- tissue_table %>% inner_join(genes, "GENE")

return(tissue_table_genes)
}