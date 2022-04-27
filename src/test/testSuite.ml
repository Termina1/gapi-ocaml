open OUnit

let core_tests = [ TestCore.suite ]

let model_tests =
  [
    TestDate.suite;
    TestCalendarV3Model.suite;
    TestPlusV1Model.suite;
    TestTasksV1Model.suite;
    TestDiscoveryV1Model.suite;
    TestUrlshortenerV1Model.suite;
    TestOauth2V2Model.suite;
    TestCustomsearchV1Model.suite;
    TestAnalyticsV3Model.suite;
    TestPagespeedonlineV1Model.suite;
    TestBloggerV2Model.suite;
    TestSiteVerificationV1Model.suite;
    TestAdsenseV1_1Model.suite;
    TestBigqueryV2Model.suite;
    TestMediaResource.suite;
    TestDriveV2Model.suite;
    TestDriveV3Model.suite;
    TestError.suite;
    TestServiceAccountCredentials.suite;
    TestJwtServiceAccount.suite;
  ]

let service_tests =
  [
    TestAuth.suite;
    TestCalendarV3Service.suite;
    TestPlusV1Service.suite;
    TestTasksV1Service.suite;
    TestDiscoveryV1Service.suite;
    TestUrlshortenerV1Service.suite;
    TestOauth2V2Service.suite;
    TestCustomsearchV1Service.suite;
    TestAnalyticsV3Service.suite;
    TestPagespeedonlineV1Service.suite;
    TestBloggerV2Service.suite;
    TestSiteVerificationV1Service.suite;
    TestAdsenseV1_1Service.suite;
    TestBigqueryV2Service.suite;
    TestDriveV2Service.suite;
    TestOAuth2ServiceAccount.suite;
    TestOAuth2Devices.suite;
  ]

let build_service_test_list service =
  let service_suite =
    match service with
    | "auth" -> [ TestAuth.suite ]
    | "calendar-v3" -> [ TestCalendarV3Service.suite ]
    | "plus" -> [ TestPlusV1Service.suite ]
    | "tasks" -> [ TestTasksV1Service.suite ]
    | "discovery" -> [ TestDiscoveryV1Service.suite ]
    | "urlshortener" -> [ TestUrlshortenerV1Service.suite ]
    | "oauth2" -> [ TestOauth2V2Service.suite ]
    | "customsearch" -> [ TestCustomsearchV1Service.suite ]
    | "analytics" -> [ TestAnalyticsV3Service.suite ]
    | "pagespeedonline" -> [ TestPagespeedonlineV1Service.suite ]
    | "blogger" -> [ TestBloggerV2Service.suite ]
    | "siteVerification" -> [ TestSiteVerificationV1Service.suite ]
    | "adsense" -> [ TestAdsenseV1_1Service.suite ]
    | "bigquery" -> [ TestBigqueryV2Service.suite ]
    | "drive" -> [ TestDriveV2Service.suite ]
    | "oa2serv" -> [ TestOAuth2ServiceAccount.suite ]
    | "oa2devices" -> [ TestOAuth2Devices.suite ]
    | _ -> failwith ("Service not supported: " ^ service)
  in
  service_suite

let build_suite_from_list test_list =
  "Google API OCaml client test suite" >::: test_list

let _ =
  let test_list = ref (core_tests @ model_tests) in
  let ounit2_specs =
    [
      ("-verbose", Arg.Unit (fun _ -> ()), "See ounit2 doc");
      ("-only-test", Arg.String (fun _ -> ()), "See ounit2 doc");
      ("-list-test", Arg.String (fun _ -> ()), "See ounit2 doc");
    ]
  in
  let arg_specs =
    Arg.align
      [
        ( "-service",
          Arg.String
            (fun service -> test_list := build_service_test_list service),
          "svc Google service to test (auth, oa2serv, calendar, calendar-v3, \
           plus, tasks, discovery, urlshortener, oauth2, customsearch, \
           analytics, pagespeedonline, blogger, siteVerification, adsense, \
           bigquery, documents)" );
        ( "-all",
          Arg.Unit
            (fun () -> test_list := core_tests @ model_tests @ service_tests),
          " Run all tests" );
      ]
  in
  let _ =
    Arg.parse (arg_specs @ ounit2_specs)
      (fun _ -> ())
      ("Usage: " ^ Sys.argv.(0) ^ " [-service svc] [-all] [ounit2 arguments]")
  in
  let _ =
    (* Reset argument counter, to let OUnit reparse arguments *)
    Arg.current := 0
  in
  let suite = build_suite_from_list !test_list in
  OUnit.run_test_tt_main ~arg_specs suite
