SELECT TOP (1000)
  concept_name
  ,concept_code
  ,'TRUE'        as desired
  ,''            as comments
  ,domain_id
  ,vocabulary_id
  ,concept_class_id
  ,concept_id
  --,standard_concept
  --,valid_start_date
  --,valid_end_date
  --,invalid_reason
FROM cdw_omop_1.v6.concept
WHERE
  (
    (
      domain_id = 'Condition'
      and
      vocabulary_id = 'ICD10CM'
    )
    or
    (
      domain_id = 'Measurement'
    )
    -- we're not messing with any observation concepts (for now at least)
  )
  and
  concept_id in (
    3085175
    ,4231384
    ,4110042
    ,40487460
    ,44792686
    ,40491985
    ,45601120
    ,3240198
    ,45586659
    ,45755451
    ,45755450
    ,44825473
    ,4191380
    ,3085181
    ,4112662
    ,3267859
    ,3298499
    ,40448821
    ,3390868
    ,3164021
    ,45755449
    ,40316527
    ,40323859
    ,46274061
    ,3239384
    ,44796183
    ,3343472
    ,3257768
    ,3286230
    ,4148971
    ,4110634
    ,44837133
    ,46270127
    ,46270123
    ,40492881
    ,44824286
    ,3250492
    ,46270128
    ,44826676
    ,3574568
    ,4186568
    ,607085
    ,45939126
    ,3329708
    ,44797578
    ,3241374
    ,40488817
    ,3160849
    ,3251973
    ,3526661
    ,45572160
    ,3105899
    ,3279035
    ,40276903
    ,3403147
    ,4224058
    ,3432423
    ,3188945
    ,3568183
    ,3333082
    ,3530039
    ,3124961
    ,46270125
    ,3288128
    ,3519185
    ,3163538
    ,45768913
    ,765607
    ,35207930
    ,44783879
    ,45763831
    ,45763832
    ,40600349
    ,3072699
    ,3265315
    ,3085898
    ,45581842
    ,4141658
    ,40484544
    ,3266409
    ,4236145
    ,3366252
    ,3085182
    ,3422435
    ,45945626
    ,3270471
    ,45771050
    ,40389698
    ,3233374
    ,46270121
    ,3327691
    ,46269769
    ,3085649
    ,4189256
    ,45591545
    ,3180959
    ,3180449
    ,320752
    ,3102500
    ,40448822
    ,46273463
    ,3374002
    ,40345755
    ,3189304
    ,3574549
    ,3574544
    ,44819746
    ,3122257
    ,44827821
    ,3262837
    ,45915560
    ,45601118
    ,4183609
    ,3467232
    ,36676221
    ,3383525
    ,3281138
    ,40492869
    ,3338358
    ,3164121
    ,3085178
    ,45562440
    ,4014332
    ,40492886
    ,40488386
    ,3219538
    ,3424848
    ,3248202
    ,45890944
    ,4223225
    ,3425707
    ,46270120
    ,4013870
    ,40365652
    ,3382749
    ,44830113
    ,3536147
    ,4304374
    ,4112665
    ,3312092
    ,36714388
    ,3468328
    ,46270126
    ,37394477
    ,4110511
    ,3157727
    ,3141618
    ,3342558
    ,3124954
    ,3405480
    ,44807171
    ,43530905
    ,37394476
    ,3343128
    ,35207931
    ,3655653
    ,3270671
    ,46269741
    ,4261801
    ,3519316
    ,3182572
    ,40488301
    ,3564022
    ,3105650
    ,45586660
    ,3243227
    ,765180
    ,45939125
    ,37395789
    ,3405529
    ,44792676
    ,8689
    ,3416198
    ,3293952
    ,4224092
    ,3159354
    ,3227284
    ,762597
    ,45920257
    ,4248398
    ,42537960
    ,1569460
    ,3085176
    ,40488816
    ,3407058
    ,45572159
    ,40492882
    ,4231383
    ,46270124
    ,4226102
    ,3085179
    ,36674353
    ,42872723
    ,3566594
    ,45581841
    ,44792677
    ,256723
    ,37395787
    ,36676220
    ,3215445
    ,45936296
    ,4299935
    ,37394479
    ,45596276
    ,3362716
    ,763012
    ,40395528
    ,44824284
    ,45543254
    ,4262073
    ,3329123
    ,37395788
    ,46272714
    ,3365486
    ,761948
    ,3124962
    ,3547987
    ,3227850
    ,3261451
    ,4208931
    ,36676238
    ,40316526
    ,3451137
    ,45543255
    ,36676233
    ,45601121
    ,4176911
    ,3531376
    ,40488818
    ,312664
    ,37395790
    ,3547909
    ,3294989
    ,40316532
    ,3085177
    ,3159774
    ,3290699
    ,40492902
    ,3278113
    ,3547910
    ,1569459
    ,45769799
    ,3407051
    ,36716296
    ,3093117
    ,3574545
    ,37395774
    ,46272887
    ,3396161
    ,3267177
    ,3364659
    ,3124956
    ,3289342
    ,4301755
    ,44832420
    ,45581844
    ,3424321
    ,3105895
    ,45591546
    ,3531375
    ,40316529
    ,4014334
    ,46269736
    ,42872724
    ,3443649
    ,45920258
    ,45533544
    ,44798590
    ,44825471
    ,44827820
    ,4207452
    ,3185307
    ,40636737
    ,3356470
    ,3314007
    ,3332434
    ,36714570
    ,4196701
    ,3280097
    ,37016926
    ,37394478
    ,40493325
    ,40493340
    ,4144103
    ,3569788
    ,44825472
    ,45950028
    ,46269705
    ,45936297
    ,3405685
    ,44796184
    ,3266479
    ,3315961
    ,45765521
    ,46269737
    ,3421962
    ,3216460
    ,4237779
    ,4013424
    ,45581843
    ,3429586
    ,3124958
    ,3413524
    ,3085174
    ,3161617
    ,3250190
    ,3159522
    ,40487461
    ,3574543
    ,3292499
    ,45915096
    ,765319
    ,45773008
    ,3123025
    ,3124953
    ,46274030
    ,4248810
    ,3466363
    ,4112825
    ,3441272
    ,4013425
    ,3064799
    ,1569464
    ,4012982
    ,314979
    ,37111241
    ,3547998
    ,3063800
    ,46269706
    ,3547911
    ,4262075
    ,4235537
    ,3409840
    ,37396162
    ,40489416
    ,3334938
    ,40492875
    ,3392470
    ,44802951
    ,3124955
    ,3226841
    ,3327169
    ,3264565
    ,45928942
    ,40488814
    ,3273275
    ,40492910
    ,4146943
    ,4222235
    ,3466923
    ,45927132
    ,40488300
    ,3529062
    ,3221589
    ,40642055
    ,3105896
    ,3573522
    ,4112824
    ,3355359
    ,44826675
    ,40488815
    ,3248572
    ,3442542
    ,3244482
    ,3234756
    ,4252885
    ,3460861
    ,3181650
    ,44830112
    ,40468330
    ,3331622
    ,4230513
    ,3225826
    ,3393198
    ,45757528
    ,45586661
    ,1569461
    ,45576945
    ,44792675
    ,45567259
    ,3306004
    ,761937
    ,3223723
    ,37395786
    ,45601119
    ,3139033
    ,45913703
    ,1569463
    ,3124959
    ,3168439
    ,40395532
    ,4234893
    ,45557617
    ,46270318
    ,4080680
    ,37016981
    ,4012884
    ,4142111
    ,40483537
    ,45944963
    ,3159955
    ,4013426
    ,40493330
    ,4112663
    ,3139032
    ,3085180
    ,40491950
    ,3567566
    ,45538475
    ,3414908
    ,3339538
    ,4012983
    ,1569458
    ,3144774
    ,46270122
    ,40483112
    ,46270491
    ,1569462
    ,44824285
    ,44832419
    ,3086682
    ,3217657
    ,3164086
    ,3405940
    ,4266367
    ,3124957
    ,3442824
    ,3391557
    ,3291593
    ,3182375
    ,3358726
    ,3547912
    ,40492974
    ,45581845
    ,3235807
    ,3376116
    ,3180004
    ,3397990
    ,46269742
    ,4108226
    ,45936298
    ,42872489
    ,3123027
    ,3087742
    ,3357604
    ,3238749
    ,4112664
    ,45936295
    ,4110512
    ,4234892
    ,4110043
    ,3564524
    ,36676232
  )
ORDER BY
  domain_id
  ,concept_name