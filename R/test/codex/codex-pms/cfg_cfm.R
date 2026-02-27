cfg <- list(

    cashTransactions = list(
        file = list(
            pattern = "521\\.mesp",
            cols_raw = c(
                "numero_d_operation", "numero_de_client", "numero_d_ordre_du_compte",
                "devise_du_compte", "numero_iban", "type_d_operation",
                "debit_credit", "description_du_mouvement", "monnaie_operation",
                "monnaie_de_decompte", "date_operation", "date_valeur",
                "date_de_comptabilisation", "montant_brut", "frais", "montant_net",
                "texte", "extournes", "operations_extournees", "communications_cac",
                "reference_remise_telematique_banque",
                "reference_remise_telematique_client",
                "cours_de_change", "v19"
            ),
            date_cols = c("date_operation", "date_valeur", "date_de_comptabilisation")
        ),
        db = list(
            table = "cashtransactions",
            keys = c("numero_d_operation", "numero_de_client",
                     "date_operation", "devise_du_compte", "numero_iban")
        )
    ),

    securityPositions = list(
        file = list(
            pattern = "532\\.poti",
            cols_raw = c(
                "no_d_intervenant", "no_de_dossier", "no_de_valeur", "no_isin",
                "libelle_du_nom_du_titre", "ponderation_dans_le_portefeuille",
                "libelle_du_genre_de_titre", "monnaie_de_negociation",
                "quantite", "cours_en_devise_de_la_position",
                "valeur_brute_d_achat_en_monnaie_de_position",
                "code_genre_de_titre", "valeur_brute_d_achat_en_monnaie_locale",
                "date_du_cours_d_estimation",
                "cours_moyen_d_achat_en_monnaie_de_position",
                "cours_moyen_d_achat_en_monnaie_de_performance",
                "interets_courus_en_monnaie_de_position",
                "cours_du_titre_exprime_en_pct_ou_en_monnaie",
                "date_d_arrete", "v20", "v21", "v22"
            ),
            date_cols = c("date_du_cours_d_estimation", "date_d_arrete")
        ),
        db = list(
            table = "securitypositions",
            keys = c("no_de_dossier", "no_isin",
                     "date_d_arrete",
                     "monnaie_de_negociation",
                     "quantite",
                     "cours_en_devise_de_la_position")
        )
    ),

    cashPositions = list(
        file = list(
            pattern = "523\\.poes",
            cols_raw = c(
                "no_d_intervenant", "no_d_ordre_de_compte",
                "libelle_du_type_de_compte", "devise_du_compte",
                "ponderation_du_compte_dans_le_portefeuille",
                "solde_en_devise_de_position", "solde_en_devise_de_perf",
                "interets_courus_en_monnaie_de_position", "iban",
                "detail_position_cat", "devise_position_cat",
                "date_d_arrete", "v19", "v20"
            ),
            date_cols = c("date_d_arrete")
        ),
        db = list(
            table = "cashpositions",
            keys = c("no_d_intervenant", "iban",
                     "devise_du_compte", "date_d_arrete")
        )
    ),

    clientDetails = list(
        file = list(
            pattern = "524\\.intv",
            cols_raw = c(
                "numero_de_client", "libelle_du_client",
                "monnaie_de_reference", "type_de_client",
                "sous_profil_de_gestion", "lettre_cle",
                "performance_mwr_ytd", "v20"
            ),
            date_cols = c("as_of_date"),
            add_filename_date = TRUE,
            filename_date_col = "as_of_date"
        ),
        db = list(
            table = "clientdetails",
            keys = c("numero_de_client",
                     "monnaie_de_reference",
                     "as_of_date")
        )
    ),

    fxRates = list(
        file = list(
            pattern = "525\\.crsc",
            cols_raw = c(
                "devise", "date", "cours_de_change", "v20"
            ),
            date_cols = c("date")
        ),
        db = list(
            table = "fxrates",
            keys = c("devise", "date")
        )
    ),

    securities = list(
        file = list(
            pattern = "526\\.vale",
            cols_raw = c(
                "numero_de_valeur_telekurs", "no_d_intervenant",
                "numero_isin", "genre_de_titre", "nom_du_titre",
                "pays_de_l_emetteur", "place_boursiere",
                "monnaie_du_titre", "date_d_emission",
                "date_d_echeance", "valeur_nominale",
                "taux_d_interets", "monnaie_du_coupon",
                "dates_coupons", "nombre_de_coupons",
                "cours_d_estimation", "libelle_du_genre_de_titre",
                "date_du_cours_d_estimation", "v19"
            ),
            date_cols = c("as_of_date",
                          "date_d_emission",
                          "date_d_echeance",
                          "date_du_cours_d_estimation"),
            add_filename_date = TRUE,
            filename_date_col = "as_of_date"
        ),
        db = list(
            table = "securities",
            keys = c("numero_isin",
                     "date_du_cours_d_estimation",
                     "as_of_date")
        )
    ),

    fxTransactions = list(
        file = list(
            pattern = "527\\.mrfc",
            cols_raw = c(
                "cle_unique_de_l_operation", "numero_de_client",
                "numero_d_ordre_du_compte", "devise_du_compte",
                "type_d_operation", "debit_credit",
                "devise_operation", "devise_de_decompte",
                "date_operation", "date_valeur",
                "montant", "texte", "date_d_echeance",
                "extournes", "operation_extournee",
                "cours_spot", "cours_a_terme",
                "lettre_cle", "devise_directrice", "v19"
            ),
            date_cols = c("date_operation",
                          "date_valeur",
                          "date_d_echeance")
        ),
        db = list(
            table = "fxtransactions",
            keys = c("cle_unique_de_l_operation",
                     "numero_de_client",
                     "date_operation")
        )
    ),

    couponTransactions = list(
        file = list(
            pattern = "528\\.mcps",
            cols_raw = c(
                "cle_unique_operation", "numero_client",
                "numero_d_ordre_du_compte", "devise",
                "numero_de_valeur", "type_d_operation",
                "numero_du_coupon", "monnaie_d_operation",
                "monnaie_du_decompte", "date_d_operation",
                "date_valeur", "date_de_comptabilisation",
                "quantite", "taux", "montant_brut",
                "frais", "taxes_impots_anticipes",
                "impots_recuperables", "montant_net",
                "libelle_de_l_operation",
                "texte_extournes", "operation_extournee",
                "code_isin", "prelevements_precompte_mobilier",
                "v19"
            ),
            date_cols = c("date_d_operation",
                          "date_valeur",
                          "date_de_comptabilisation")
        ),
        db = list(
            table = "coupontransactions",
            keys = c("cle_unique_operation",
                     "numero_client",
                     "date_d_operation")
        )
    ),

    depositTransactions = list(
        file = list(
            pattern = "529\\.mfid",
            cols_raw = c(
                "cle_unique_de_l_operation", "numero_de_client",
                "numero_d_ordre_du_compte", "devise_du_compte",
                "libelle_du_type_d_operation", "type_de_contrat",
                "devise_de_contrat", "devise_d_operation",
                "date_d_execution", "date_valeur",
                "date_d_echeance", "montant_operation",
                "taux_d_interets", "methode_de_calcul_interets",
                "montant_des_interets", "frais",
                "courtage", "commission",
                "montant_net", "texte",
                "extournes", "operation_extournee",
                "v19"
            ),
            date_cols = c("date_d_execution",
                          "date_valeur",
                          "date_d_echeance")
        ),
        db = list(
            table = "deposittransactions",
            keys = c("cle_unique_de_l_operation",
                     "numero_de_client",
                     "date_valeur")
        )
    ),

    fiduPositions = list(
        file = list(
            pattern = "531\\.pofi",
            cols_raw = c(
                "cle_unique_de_l_operation", "numero_de_client",
                "numero_d_ordre_du_compte", "devise_du_compte",
                "libelle_du_type_d_operation", "type_de_contrat",
                "devise_de_contrat", "devise_d_operation",
                "date_d_execution", "date_valeur",
                "date_d_echeance", "montant_operation",
                "taux_d_interets", "methode_de_calcul_interets",
                "montant_des_interets", "frais",
                "courtage", "commission",
                "montant_net", "texte",
                "extournes", "operation_extournee",
                "v19"
            ),
            date_cols = c("date_d_execution",
                          "date_valeur",
                          "date_d_echeance")
        ),
        db = list(
            table = "fidupositions",
            keys = c("cle_unique_de_l_operation",
                     "numero_de_client")
        )
    )

)
