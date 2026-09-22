V21 CASE-SPECIFIC DIAGRAM UPDATE

INTERSTATE BICYCLIST CRASH EXPLORER - V19 API STABLE

Recommended run procedure
1. Extract the ZIP into a new folder.
2. Open Interstate_Bicyclist_Crash_Explorer.Rproj.
3. Open app.R and click Run App, or run shiny::runApp().

Design
- Study CCA uses the validated manuscript Dark/Daylight CCA and SHAP outputs.
- Custom CCA remains interactive.
- Selected-crash cluster explanation includes hard CCA assignment, relative CCA proximity, and residual-profile alignment.
- Street View uses the stable no-key preview/link.
- Roadway/site suggestions are derived from mapped roadway context when available and are always editable by the user.
- Countermeasure selection remains constrained to manuscript-linked countermeasures and only one selected countermeasure is displayed.
- The case-specific roadway diagram uses mapped roadway form/lane context where available and clearly labels conceptual elements.
- An optional private API may refine the wording of the explanation, but it cannot change analytical results.

Privacy
See PRIVATE_API_SETUP.txt. Never paste an API key into app.R or GitHub.

If a mapped roadway request fails because the public map service is temporarily unavailable, the app falls back to the interstate study context and manual site confirmation rather than stopping.


V19 diagram revision:
- CCA evidence is labeled as cluster-based relevance, not direct manuscript linkage.
- SHAP evidence is labeled as validated SHAP evidence.
- Street View and the crash/countermeasure diagram are stacked so the diagram uses full width.
- The diagram uses a larger top-view vehicle graphic and large text.
- PBCAT=Others is shown as an uncertain maneuver rather than an invented crash trajectory.
- Navigation/wayfinding shows an upstream route decision to an appropriate permitted bicycle route; no shared-use-path assumption is made.


V21 ROADWAY-CONTEXT CHANGE
==========================
V21 no longer treats mapped lane tags or motorway classification as validated lane/divided geometry. Confirm geometry in the Crash + Street View tab or populate data/site_context_validated.csv. See SITE_CONTEXT_REVIEW_GUIDE.txt.

CAR ICON
========
The mechanism diagram uses a simplified front-view car asset based on the user-provided reference image (www/car_icon.png).
