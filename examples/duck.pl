
:- use_module(library(chr)).
:- chr_constraint argument/2, go/0, '¬'/1.
:- initialization main.
:- op(900, fx, ¬).

write_constraint_store :-
	find_chr_constraint(C),
	writeln(C),
	fail.
write_constraint_store.
	
main :-
  assumptions,
  write_constraint_store,
  halt(0).


:- chr_constraint notConsentRequired/1,
   extendsScope/2,
   li/1,
   passive/2,
   partOf/2,
   sourceScope/2,
   compatiblePurpose/2,
   duplicate/2,
   docConsentRequired,
   useScope/2,
   notDocConsentRequired,
   resultScope/2,
   scope/2,
   greaterScope/2,
   notPii/1,
   smallerOrEqualScope/2,
   equivalentScope/2,
   notLi/1,
   consentRequired/1,
   id/2,
   action/2,
   dataCategory/2,
   dataUseStatement/1,
   qualifier/2,
   kindOf/2,
   lesserScope/2,
   pii/1.

selectors @ dataUseStatement(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive)) ==> useScope(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),UseScope),qualifier(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),Qualifier),dataCategory(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),DataCategory),sourceScope(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),SourceScope),action(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),Action),resultScope(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),ResultScope),id(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),ID),passive(dus(UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive),Passive),argument(selectors,[UseScope,Qualifier,DataCategory,SourceScope,Action,ResultScope,ID,Passive]).
kindOfTransitivity @ kindOf(X,Y),kindOf(Y,Z) ==> kindOf(X,Z),argument(kindOfTransitivity,[X,Y,Z]).
partOfTransitivity @ partOf(X,Y),partOf(Y,Z) ==> partOf(X,Z),argument(partOfTransitivity,[X,Y,Z]).
qualifier1 @ qualifier(S,identified_data) ==> qualifier(S,unqualified_or_identified),argument(qualifier1,[S]).
qualifier2 @ qualifier(S,unqualified) ==> qualifier(S,unqualified_or_identified),argument(qualifier2,[S]).
scope1 @ scope(S,csp_products) ==> scope(S,csp_services),argument(scope1,[S]).
scope2 @ scope(S,csp_services) ==> scope(S,services_agreement),argument(scope2,[S]).
scope3 @ scope(S,services_agreement) ==> scope(S,service),argument(scope3,[S]).
scope4 @ scope(S,service) ==> scope(S,cability),argument(scope4,[S]).
category0 @ dataCategory(S,X),kindOf(X,Y) ==> dataCategory(S,Y),argument(category0,[S,X,Y]).
category1 @ go ==> kindOf(derived_data_user_telemetry,derived_data_user),argument(category1,[]).
category2 @ go ==> kindOf(derived_data_user_connectivity,derived_data_user),argument(category2,[]).
category3 @ go ==> kindOf(derived_data_user_usage,derived_data_user),argument(category3,[]).
category4 @ go ==> kindOf(derived_data_user_demographic,derived_data_user),argument(category4,[]).
category5 @ go ==> kindOf(derived_data_user_profiling,derived_data_user),argument(category5,[]).
category6 @ go ==> kindOf(derived_data_user_content,derived_data_user),argument(category6,[]).
category7 @ go ==> kindOf(derived_data_user_browsing,derived_data_user),argument(category7,[]).
category8 @ go ==> kindOf(derived_data_user_search,derived_data_user),argument(category8,[]).
category9 @ go ==> kindOf(derived_data_user_location,derived_data_user),argument(category9,[]).
category10 @ go ==> kindOf(derived_data_user_social,derived_data_user),argument(category10,[]).
category11 @ go ==> kindOf(derived_data_user_biometric,derived_data_user),argument(category11,[]).
category12 @ go ==> kindOf(derived_data_user_contact,derived_data_user),argument(category12,[]).
category13 @ go ==> kindOf(derived_data_user_environmental,derived_data_user),argument(category13,[]).
category14 @ go ==> kindOf(customer_content_credentials,customer_content),argument(category14,[]).
category15 @ go ==> kindOf(customer_content_contact,customer_content),argument(category15,[]).
category16 @ go ==> kindOf(customer_content_health,customer_content),argument(category16,[]).
category17 @ go ==> kindOf(customer_content_genetic,customer_content),argument(category17,[]).
category18 @ go ==> kindOf(customer_content_biometric,customer_content),argument(category18,[]).
category19 @ go ==> kindOf(customer_content_children,customer_content),argument(category19,[]).
category20 @ go ==> kindOf(customer_content_opinions,customer_content),argument(category20,[]).
category21 @ go ==> kindOf(customer_content_financial,customer_content),argument(category21,[]).
category22 @ go ==> kindOf(derived_data_user,derived_data),argument(category22,[]).
category23 @ go ==> kindOf(derived_data_organization,derived_data),argument(category23,[]).
category24 @ go ==> kindOf(provider_data_authentication,provider_data),argument(category24,[]).
category25 @ go ==> kindOf(provider_data_operations,provider_data),argument(category25,[]).
category26 @ go ==> kindOf(account_data_customer,account_data),argument(category26,[]).
category27 @ go ==> kindOf(account_data_payment,account_data),argument(category27,[]).
action0 @ action(S,X),kindOf(X,Y) ==> action(S,Y),argument(action0,[S,X,Y]).
action1 @ go ==> kindOf(market,market_advertise_promote),argument(action1,[]).
action2 @ go ==> kindOf(advertise,market_advertise_promote),argument(action2,[]).
action3 @ go ==> kindOf(promote,market_advertise_promote),argument(action3,[]).
action4 @ go ==> kindOf(market_contextual,market),argument(action4,[]).
action5 @ go ==> kindOf(market_personalization,market),argument(action5,[]).
action6 @ go ==> kindOf(advertise_contextual,advertise),argument(action6,[]).
action7 @ go ==> kindOf(advertise_personalization,advertise),argument(action7,[]).
action8 @ go ==> kindOf(promote_contextual,promote),argument(action8,[]).
action9 @ go ==> kindOf(promote_personalization,promote),argument(action9,[]).
action10 @ go ==> kindOf(share_provide,share),argument(action10,[]).
docConsent1 @ go ==> notDocConsentRequired,argument(docConsent1,[]).
docConsent2 @ consentRequired(S) ==> docConsentRequired,argument(docConsent2,[S]).
pii0 @ dataUseStatement(S) ==> pii(S),argument(pii0,[S]).
pii3 @ qualifier(S,unlinked_data) ==> notPii(S),argument(pii3,[S]).
pii4 @ qualifier(S,anonymized_data) ==> notPii(S),argument(pii4,[S]).
pii5 @ qualifier(S,aggregated_data) ==> notPii(S),argument(pii5,[S]).
pii6 @ qualifier(S,pseudonymized_data),dataCategory(S,derived_data_organization) ==> notPii(S),argument(pii6,[S]).
pii7 @ qualifier(S,pseudonymized_data),dataCategory(S,provider_data_authentication) ==> notPii(S),argument(pii7,[S]).
pii8 @ qualifier(S,unqualified_or_identified),dataCategory(S,derived_data_organization) ==> notPii(S),argument(pii8,[S]).
pii9 @ qualifier(S,unqualified_or_identified),dataCategory(S,provider_data_authentication) ==> notPii(S),argument(pii9,[S]).
pii11 @ qualifier(S,unqualified),dataCategory(S,derived_data_organization) ==> notPii(S),argument(pii11,[S]).
pii12 @ qualifier(S,unqualified),dataCategory(S,provider_data_authentication) ==> notPii(S),argument(pii12,[S]).
li0 @ dataUseStatement(S) ==> notLi(S),argument(li0,[S]).
li1 @ action(S,provide),resultScope(S,capability) ==> li(S),argument(li1,[S]).
li2 @ action(S,provide),resultScope(S,service) ==> li(S),argument(li2,[S]).
li3 @ action(S,improve),resultScope(S,capability) ==> li(S),argument(li3,[S]).
li4 @ action(S,improve),resultScope(S,service) ==> li(S),argument(li4,[S]).
li5 @ action(S,improve),resultScope(S,services_agreement) ==> li(S),argument(li5,[S]).
li6 @ action(S,improve),resultScope(S,csp_services) ==> li(S),argument(li6,[S]).
li7 @ action(S,improve),resultScope(S,csp_products) ==> li(S),argument(li7,[S]).
li8 @ action(S,personalize),resultScope(S,capability) ==> li(S),argument(li8,[S]).
li9 @ action(S,personalize),resultScope(S,service) ==> li(S),argument(li9,[S]).
li10 @ action(S,upgrades),resultScope(S,capability) ==> li(S),argument(li10,[S]).
li11 @ action(S,upgrades),resultScope(S,service) ==> li(S),argument(li11,[S]).
li12 @ action(S,upgrades),resultScope(S,services_agreement) ==> li(S),argument(li12,[S]).
li13 @ action(S,market_advertise_promote),resultScope(S,capability) ==> li(S),argument(li13,[S]).
li14 @ action(S,market_advertise_promote),resultScope(S,service) ==> li(S),argument(li14,[S]).
equivalentScope0 @ resultScope(S1,S),resultScope(S2,S) ==> equivalentScope(S1,S2),argument(equivalentScope0,[S1,S2]).
equivalentScope1 @ resultScope(S1,capability),resultScope(S2,service) ==> equivalentScope(S1,S2),argument(equivalentScope1,[S1,S2]).
equivalentScope2 @ resultScope(S1,services_agreement),resultScope(S2,csp_services) ==> equivalentScope(S1,S2),argument(equivalentScope2,[S1,S2]).
equivalentScope3 @ resultScope(S1,third_party_partners),resultScope(S2,third_party_services) ==> equivalentScope(S1,S2),argument(equivalentScope3,[S1,S2]).
equivalentScope4 @ resultScope(S1,third_party_services),resultScope(S2,third_party_partners) ==> equivalentScope(S1,S2),argument(equivalentScope4,[S1,S2]).
smallerOrEqualScope1 @ resultScope(S1,C),resultScope(S2,C) ==> smallerOrEqualScope(S1,S2),argument(smallerOrEqualScope1,[S1,S2,C]).
smallerOrEqualScope2 @ smallerOrEqualScope(S1,S2) \ smallerOrEqualScope(S1,S2) <=> true,argument(smallerOrEqualScope2,[S1,S2]).
smallerOrEqualScope3 @ smallerOrEqualScope(S1,S2),smallerOrEqualScope(S2,S3) ==> smallerOrEqualScope(S1,S3),argument(smallerOrEqualScope3,[S1,S2,S3]).
smallerOrEqualScope4 @ resultScope(S1,P1),resultScope(S2,P2),lesserScope(P1,P2) ==> smallerOrEqualScope(S1,S2),argument(smallerOrEqualScope4,[S1,S2,P1,P2]).
lesserScope1 @ go ==> lesserScope(capability,service),lesserScope(service,services_agreement),lesserScope(services_agreement,csp_services),lesserScope(csp_services,csp_products),argument(lesserScope1,[]).
compatiblePurposeDuplicates @ compatiblePurpose(S1,S2) \ compatiblePurpose(S1,S2) <=> true,argument(compatiblePurposeDuplicates,[S1,S2]).
compatiblePurpose1 @ action(S1,A),action(S2,A),equivalentScope(S1,S2) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose1,[S1,S2,A]).
compatiblePurpose2 @ action(S1,provide),action(S2,improve),smallerOrEqualScope(S2,csp_products) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose2,[S1,S2]).
compatiblePurpose3 @ action(S1,provide),action(S2,upgrades),equivalentScope(S1,S2) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose3,[S1,S2]).
compatiblePurpose4 @ action(S1,provide),action(S2,market_advertise_promote),equivalentScope(S1,S2) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose4,[S1,S2]).
compatiblePurpose5 @ action(S1,improve),action(S2,improve),smallerOrEqualScope(S2,csp_products) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose5,[S1,S2]).
compatiblePurpose6 @ action(S1,personalize),action(S2,personalize),smallerOrEqualScope(S2,services_agreement) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose6,[S1,S2]).
compatiblePurpose7 @ action(S1,personalize),action(S2,market_advertise_promote),smallerOrEqualScope(S2,services_agreement) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose7,[S1,S2]).
compatiblePurpose8 @ action(S1,upgrades),action(S2,upgrades),smallerOrEqualScope(S2,services_agreement) ==> compatiblePurpose(S1,S2),argument(compatiblePurpose8,[S1,S2]).
consentRequired0 @ consentRequired(S1) \ consentRequired(S1) <=> true,argument(consentRequired0,[S1]).
consentRequired1 @ notConsentRequired(S1) \ notConsentRequired(S1) <=> true,argument(consentRequired1,[S1]).
consentRequired2 @ dataUseStatement(S) ==> consentRequired(S),argument(consentRequired2,[S]).
consentRequired3 @ notPii(S) ==> notConsentRequired(S),argument(consentRequired3,[S]).
consentRequired4 @ pii(S),li(S) ==> notConsentRequired(S),argument(consentRequired4,[S]).
consentRequired5 @ pii(S1),pii(S2),li(S2),compatiblePurpose(S1,S2) ==> notConsentRequired(S1),argument(consentRequired5,[S1,S2]).


assumptions :- 
    dataUseStatement(dus(capability,identified_data,provider_data_authentication,capability,provide,capability,a6bf5e759ee6f4177ac862579a1f89570,false)),
      dataUseStatement(dus(capability,pseudonymized_data,provider_data_operations,capability,provide,capability,abfd5f3573ea94310bf57d282d02789f4,false)),
      dataUseStatement(dus(capability,aggregated_data,derived_data_user_social,capability,provide,capability,acc18624e860c4feaaa4fc95f8f107ad3,false)),
      dataUseStatement(dus(capability,aggregated_data,derived_data_user_browsing,capability,provide,capability,a49b72f578f2f4124a496f408f79c14aa,false)),
      dataUseStatement(dus(capability,identified_data,customer_content_credentials,capability,provide,capability,a3de0b1dfd69c492487adce5a39dc7795,false)),
      dataUseStatement(dus(capability,anonymized_data,customer_content_children,capability,provide,capability,a382c3ac6971c4f45b46849548ce339d9,false)),
      dataUseStatement(dus(capability,unlinked_data,account_data_payment,capability,provide,capability,a94acb27e063746e89853999eabfba608,false)),
      dataUseStatement(dus(capability,unlinked_data,customer_content_financial,capability,provide,capability,af9f7160067be4a82b078f6d38a3ae892,false)),
      dataUseStatement(dus(capability,anonymized_data,derived_data_user_telemetry,capability,provide,services_agreement,abcce38032ccd48f1b078deb91e34c306,false)),
      dataUseStatement(dus(capability,pseudonymized_data,derived_data_user_connectivity,capability,provide,capability,adf059a79d31c4c88b05cef7376e199ac,false)),
      dataUseStatement(dus(capability,unlinked_data,customer_content_genetic,capability,provide,capability,ac74bb5e616e9449f8f504f6a8ccbde1e,false)),
    go.

