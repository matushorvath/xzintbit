    db  0

##########
first_global:
    db  10

.child_1_1:
    db  11

    out [.child_1_1]

.child_1_2:
    db  12

    out [.child_1_2]
    out [.child_1_1]

##########
second_global:
    db 20

.child_2_1:
    db  21

.child_1_2:     # duplicate name
    db  212

.child_3_1:     # no references
    db  31

+2 = .child_2_3_p2:
    db 230, 231, 232

    out [.child_2_1]
    out [.child_1_2]
    out [.child_2_3_p2]
    out [.child_2_1 + 51]
    out [.child_2_1 - 13]

.child_Lorem_ipsum_dolor_sit_amet_consectetur_adipiscing_elit_sed_do_eiusmod_tempor_incididunt_ut_labore_et_dolore_magna_aliqua_Ut_enim_ad_minim_veniam_quis_nostrud_exercitation_ullamco_laboris_nisi_ut_aliquip_ex_ea_commodo_consequat_Duis_aute_irure_dolor_in_reprehenderit_in_voluptate_velit_esse_cillum_dolore_eu_fugiat_nulla_pariatur_Excepteur_sint_occaecat_cupidatat_non_proident_sunt_in_culpa_qui_officia_deserunt_mollit_anim_id_est_laborum:
    db  999
    out [.child_Lorem_ipsum_dolor_sit_amet_consectetur_adipiscing_elit_sed_do_eiusmod_tempor_incididunt_ut_labore_et_dolore_magna_aliqua_Ut_enim_ad_minim_veniam_quis_nostrud_exercitation_ullamco_laboris_nisi_ut_aliquip_ex_ea_commodo_consequat_Duis_aute_irure_dolor_in_reprehenderit_in_voluptate_velit_esse_cillum_dolore_eu_fugiat_nulla_pariatur_Excepteur_sint_occaecat_cupidatat_non_proident_sunt_in_culpa_qui_officia_deserunt_mollit_anim_id_est_laborum]

    out .before_definition
.before_definition:
    db  -1

    out [second_global.child_2_1]
    out [second_global.child_1_2]
    out [first_global.child_1_2]
    out [second_global.child_2_1 + 23]
    out [first_global.child_1_1 - 17]

.EOF

# TODO also test linking child symbols, to make sure they are correctly relocated in second object file
