%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
{application, erlware_commons,
 [{description, "Additional standard library for Erlang"},
  {vsn, "0.2.0"},
  {modules, [
	     ec_lists,
	     ec_string,
	     ec_semver,
	     ec_dictionary,
	     ec_assoc_list,
	     ec_dict,
	     ec_gb_trees,
	     ec_rbdict,
	     ec_orddict]},
  {registered, []},
  {applications, [kernel, stdlib]}]}.
