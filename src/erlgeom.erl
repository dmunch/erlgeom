% Copyright 2011 Couchbase, Inc.
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(erlgeom).

-export([disjoint/2, from_geom/1, to_geom/1,
    topology_preserve_simplify/2, to_geom_validate/1]).

-on_load(init/0).

-include_lib("eunit/include/eunit.hrl").

init() ->
    SoName = case code:priv_dir(?MODULE) of
    {error, bad_name} ->
        case filelib:is_dir(filename:join(["..", "priv"])) of
        true ->
            filename:join(["..", "priv", "erlgeom"]);
        false ->
            filename:join(["priv", "erlgeom"])
        end;
    Dir ->
        filename:join(Dir, "erlgeom")
    end,
    (catch erlang:load_nif(SoName, 0)).

disjoint(_Geom1, _Geom2) ->
    "NIF library not loaded".

topology_preserve_simplify(_Geom1, _Tolerance) ->
    "NIF library not loaded".

% @doc Convert a GeoCouch geometry to a GEOS geometry, validate
% the structure of the geometry.
-spec to_geom_validate(Geom::{atom(), list()}) -> true|false.
to_geom_validate(Geom) ->
    case is_valid_geometry(Geom) of
        true -> to_geom(Geom);
        {false, Reason} -> throw(Reason)
    end.

% @doc Validate the structure of the geometry
-spec is_valid_geometry(Geom::{atom(), list()}) -> true|{false, string()}.
is_valid_geometry(Geom) ->
    case Geom of
    {'Point', Coords} ->
        case is_point(Coords) of
            true -> true;
            false -> {false, "Invalid Point"}
        end;
    {'LineString', Coords} ->
        is_linestring(Coords);
    {'Polygon', Coords} ->
        is_polygon(Coords);
    {'MultiPoint', Coords} ->
        case all(fun(Coord) -> is_point(Coord) end, Coords) of
        true ->
            true;
        false ->
            {false, "Not every position of the MultiPoint is a valid Point"}
        end;
    {'MultiLineString', Coords} ->
        is_polygon(Coords);
    {'MultiPolygon', Coords} ->
        case all(fun(Coord) -> is_polygon(Coord) end, Coords) of
        true ->
            true;
        false ->
            {false, "Not every Polygon is a valid one"}
        end;
    {'GeometryCollection', Coords} ->
        case all(fun(Coord) -> is_valid_geometry(Coord) end, Coords) of
        true ->
            true;
        false ->
            {false, "Not every Geometry is a valid one"}
        end;
    {GeomType, _} when is_atom(GeomType) ->
        {false, "Invalid geometry type (" ++ atom_to_list(GeomType) ++ ")"};
    _ ->
        {false, "Invalid geometry"}
    end.

-spec is_polygon(Coords::[[[number()]]]) -> true|false.
is_polygon(Coords) ->
  case all(fun(Coord) -> is_linestring(Coord) end, Coords) of
  true ->
      true;
  false ->
      {false, "Not every LineString is a valid one"}
  end.

-spec is_linestring(Coords::[[number()]]) -> true|false.
is_linestring(Coords) when length(Coords) =< 1 ->
    {false, "LineString must have more than one position"};
is_linestring(Coords) ->
    case all(fun(Coord) -> is_point(Coord) end, Coords) of
    true ->
        true;
    false ->
        {false, "Not every position of the LineString is a valid point"}
    end.

% @doc Input is a point
-spec is_point(Point::[number()]) -> true|false.
is_point([]) ->
    true;
is_point([X, Y]) when is_number(X) andalso is_number(Y) ->
    true;
is_point(_) ->
    false.

% @doc Works like lists:all, except that not only "false", but all values
% count as false. An empty list returns false. Returns also false if it
% isn't a valid list
all(_Fun, []) ->
    false;
all(Fun, List) when is_list(List) ->
    all2(Fun, List);
all(_Fun, _NotAList) ->
    false.
all2(_Fun, []) ->
    true;
all2(Fun, [H|T]) ->
    case Fun(H) of
    true ->
        all2(Fun, T);
    _ ->
        false
    end.

% @doc Convert a GeoCouch geometry to a GEOS geometry
to_geom(_Geom) ->
    "NIF library not loaded".

% @doc Convert a GEOS geometry to a GeoCouch geometry
from_geom(_Geom) ->
    "NIF library not loaded".

-ifdef(TEST).

point_test_() ->
    Pt = {'Point',[0.0, 1.1]},
    Pt1 = erlgeom:to_geom(Pt),
    [{"Point conversion works",
      ?_assertEqual(Pt, erlgeom:from_geom(Pt1))}].

linestring_test_() ->
    Ls = {'LineString', [[1.0,1.0],[5.0,5.0],[23.42,592.13],[98.2,40.2]]},
    Ls1 = erlgeom:to_geom(Ls),
    [{"LineString conversion works",
      ?_assertEqual(Ls, erlgeom:from_geom(Ls1))}].

polygon_test_() ->
    Py = {'Polygon', [[[5.2,6.3],[70.5,58.7],[0.1,20.55],[5.2,6.3]], [[10.0,20.1],[10.1,20.4],[9.8,20.2],[10.0,20.1]]]},
    Py1 = erlgeom:to_geom(Py),
    [{"Polygon conversion works",
      ?_assertEqual(Py, erlgeom:from_geom(Py1))}].

multipoint_test_() ->
    Mp = {'MultiPoint', [[1.0,1.0],[5.0,5.0]]},
    Mp1 = erlgeom:to_geom(Mp),
    Mp2 = {'MultiPoint', [[1.0,1.0],[5.0,5.0],[23.42,592.13],[98.2,40.2]]},
    Mp3 = erlgeom:to_geom(Mp2),
    [{"MultiPoint conversion works (a)",
      ?_assertEqual(Mp, erlgeom:from_geom(Mp1))},
     {"MultiPoint conversion works (b)",
      ?_assertEqual(Mp2, erlgeom:from_geom(Mp3))}].

multilinestring_test_() ->
    Ml = {'MultiLineString', [[[5.2,6.3],[70.5,58.7],[0.1,20.55],[5.2,6.3]], [[10.0,20.1],[10.1,20.4],[9.8,20.2],[10.0,20.1]]]},
    Ml1 = erlgeom:to_geom(Ml),
    [{"MultiLineString conversion works",
      ?_assertEqual(Ml, erlgeom:from_geom(Ml1))}].

multipolygon_test_() ->
    % From GeoJSON spec
    My = {'MultiPolygon',[[[[102.0, 2.0], [103.0, 2.0], [103.0, 3.0], [102.0, 3.0], [102.0, 2.0]]],[[[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0]],[[100.2, 0.2], [100.8, 0.2], [100.8, 0.8], [100.2, 0.8], [100.2, 0.2]]]]},
    My1 = erlgeom:to_geom(My),
    [{"MultiPolygon conversion works",
      ?_assertEqual(My, erlgeom:from_geom(My1))}].

geometrycollection_test_() ->
    % From GeoJSON spec
    Gc = {'GeometryCollection',[{'Point',[100.0, 0.0]},{'LineString',[[101.0, 0.0],[102.0, 1.0]]}]},
    Gc1 = erlgeom:to_geom(Gc),
    [{"GeometryCollection conversion works",
      ?_assertEqual(Gc, erlgeom:from_geom(Gc1))}].

invalid_geoms_test_() ->
    Pt = {'MultiPoint',[0.0, 10]},
    [{"Invalid geometry",
      ?_assertThrow("Not every position of the MultiPoint is a valid Point",
                    erlgeom:to_geom_validate(Pt))}].


disjoint_test_() ->
    Pt = {'Point',[3.0, 3.0]},
    Ls = {'LineString', [[1.0,1.0],[5.0,5.0]]},
    Pt1 = erlgeom:to_geom(Pt),
    Ls1 = erlgeom:to_geom(Ls),

    % Some geometries are based on the GeoJSON specification
    % http://geojson.org/geojson-spec.html (2010-08-17)
    Geoms = [
        {'Point', [100.0, 0.0]},
        {'LineString', [[100.0, 0.0], [101.0, 1.0]]},
        {'Polygon', [
            [[100.0, 0.0], [101.0, 0.0], [100.0, 1.0], [100.0, 0.0]]
        ]},
        {'Polygon', [
            [[100.0, 0.0], [101.0, 0.0], [100.0, 1.0], [100.0, 0.0]],
            [[100.2, 0.2], [100.6, 0.2], [100.2, 0.6], [100.2, 0.2]]
        ]},
        {'MultiPoint', [[100.0, 0.0], [101.0, 1.0]]},
        {'MultiLineString', [
            [[100.0, 0.0], [101.0, 1.0]],
            [[102.0, 2.0], [103.0, 3.0]]
        ]},
        {'MultiPolygon', [
            [
                [[102.0, 2.0], [103.0, 2.0], [103.0, 3.0], [102.0, 3.0], [102.0, 2.0]]
            ],[
                [[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0]],
                [[100.2, 0.2], [100.8, 0.2], [100.8, 0.8], [100.2, 0.8], [100.2, 0.2]]
            ]
        ]},
        {'GeometryCollection', [
            {'Point', [100.0, 0.0]},
            {'LineString', [[101.0, 0.0], [102.0, 1.0]]}
        ]}
    ],

    QueryGeom = erlgeom:to_geom(
        {'MultiPolygon',[[[[102.21960449216,1.66524628779],
                   [101.10998535158,2.0385856805057],
                   [100.30798339848,3.0483190208145],
                   [101.29675292969,3.3225525920246],
                   [102.83483886713,3.5418849006547],
                   [104.1641845702,2.5764772510785],
                   [103.60388183585,2.4337915164603],
                   [102.88977050775,3.2128679544585],
                   [101.58239746093,3.1251117377839],
                   [101.27478027344,2.3898851337089],
                   [101.42858886719,2.0605442798878],
                   [101.42858886719,2.0715234662377],
                   [102.21960449216,1.66524628779]]],
                 [[[100.13220214849,2.3679314141203],
                   [100.20910644536,1.3797028906988],
                   [100.58264160159,0.68768106684542],
                   [101.04406738283,1.5334617387448],
                   [101.51647949219,1.1490463293633],
                   [102.52722167964,-0.015427421675798],
                   [102.68103027339,1.2588853394239],
                   [100.13220214849,2.3679314141203]]]]}),
    Results = [erlgeom:disjoint(QueryGeom, erlgeom:to_geom(Geom))
        || Geom <- Geoms],
    [{"Geometries are not disjoint",
      ?_assertNot(erlgeom:disjoint(Pt1, Ls1))},
     {"Two geometries are not disjoint",
      ?_assertEqual([true,true,true,true,true,true, false, false], Results)}].

simplify_test_() ->
    Polygon = {'Polygon', [[[-43.59375, -0.3515625], [-31.640625, 15.8203125], [-33.046875, 25.6640625], [-37.265625, 39.7265625], [-34.453125, 67.8515625], [6.328125, 58.7109375], [21.09375, 65.0390625], [35.15625, 63.6328125], [78.046875, 63.6328125], [75.234375, 48.1640625], [65.390625, 33.3984375], [43.59375, 36.2109375], [-6.328125, 36.2109375], [-0.703125, 31.9921875], [2.109375, 5.9765625], [3.515625, -16.5234375], [-17.578125, -19.3359375], [-24.609375, -5.9765625], [-40.078125, -11.6015625], [-40.078125, -11.6015625], [-43.59375, -0.3515625]]]},
    {'Polygon', [NewCoords]} = erlgeom:topology_preserve_simplify(erlgeom:to_geom(Polygon), 30.0),
    [{"Geometry was simplified",
      ?_assertEqual(6, length(NewCoords))}].

is_point_test_() ->
    [{"Valid point with integers",
      ?_assert(is_point([20, 30]))},
     {"Valid point with floats",
      ?_assert(is_point([5.4, 10.92]))},
     {"Valid point with integer and float",
      ?_assert(is_point([9, 39.2]))},
     {"Valid point with no coords",
      ?_assert(is_point([]))},
     {"Too many coords",
      ?_assertNot(is_point([20, 30, 40]))},
     {"Too many coords",
      ?_assertNot(is_point([38.32]))}].

is_valid_geometry_test_() ->
    % Tests for Point geometries
    Point1 = {'Point', [20, 30]},
    Point2 = {'Point', [5.4, 10.92]},
    Point3 = {'Point', [9, 39.2]},
    Point4 = {'Point', []},
    Point5 = {'Point', [20, 30, 40]},
    Point6 = {'Point', [38.32]},
    [{"Valid Point with integers",
      ?_assert(is_valid_geometry(Point1))},
     {"Valid Point with floats",
      ?_assert(is_valid_geometry(Point2))},
     {"Valid Point with integer and float",
      ?_assert(is_valid_geometry(Point3))},
     {"Valid Point with no coords",
      ?_assert(is_valid_geometry(Point4))},
     {"Invalid Point: too many coords",
      ?_assertMatch({false, _}, is_valid_geometry(Point5))},
     {"Invalid Point: not enough coords",
      ?_assertMatch({false, _}, is_valid_geometry(Point6))}].

is_valid_linestring_geometry_test_() ->
    % Tests for LineString geometries
    LineString1 = {'LineString', [[20, 30], [50, 60]]},
    LineString2 = {'LineString', [[20, 30], [50, 60], [30, 10], [20, 30]]},
    LineString3 = {'LineString', [[20, 30]]},
    LineString4 = {'LineString', [[20, 40, 10], [30, 20, 5]]},
    LineString5 = {'LineString', [[20], [30]]},

    [{"Valid LineString (a)",
      ?_assert(is_valid_geometry(LineString1))},
     {"Valid LineString (b)",
      ?_assert(is_valid_geometry(LineString2))},
     {"Invalid LineString: not enough positions",
      ?_assertMatch({false, _}, is_valid_geometry(LineString3))},
     {"Invalid LineString: too many coords",
      ?_assertMatch({false, _}, is_valid_geometry(LineString4))},
     {"Invalid LineString: not enough positions",
      ?_assertMatch({false, _}, is_valid_geometry(LineString5))}].

is_valid_polygon_geometry_test_() ->
    % Tests for Polygon geometries
    Polygon1 = {'Polygon', [[[20, 30], [50, 60], [30, 10], [20, 30]]]},
    Polygon2 = {'Polygon', [[[20, 30], [50, 60], [10, 70], [20, 30]],
        [[20, 55], [28, 60], [33, 53], [25, 48], [20, 55]]]},
    Polygon3 = {'Polygon', [[20, 30], [50, 60], [30, 10], [20, 30]]},
    Polygon4 = {'Polygon', [[[20, 30], [50, 60], [30], [20, 30]]]},
    Something = {'Something', [[[20, 30], [50, 60], [30], [20, 30]]]},
    [{"Valid Polygon",
      ?_assert(is_valid_geometry(Polygon1))},
    %POLYGON((20 30, 50 60, 10 70, 20 30))
    %POLYGON((20 55, 28 60, 33 53, 25 48, 20 55))
     {"Valid Polygon with hole",
      ?_assert(is_valid_geometry(Polygon2))},
     {"Invalid Polygon: not nested enough",
      ?_assertMatch({false, _}, is_valid_geometry(Polygon3))},
     {"Invalid Polygon: invalid point somewhere",
      ?_assertMatch({false, _}, is_valid_geometry(Polygon4))},

    % Invalid GeometryType
     {"Invalid geometry type",
      ?_assertMatch({false, _}, is_valid_geometry(Something))}].

% Tests for multi-geometries
is_valid_geometry_multi_point_test_() ->
    % Tests for MultiPoint geometries
    MultiPoint1 = {'MultiPoint', [[20, 30], [50, 60]]},
    MultiPoint2 = {'MultiPoint', [[20, 30], [50, 60], [30, 10], [20, 30]]},
    MultiPoint3 = {'MultiPoint', [[20, 30]]},
    MultiPoint4 = {'MultiPoint', [[20, 40, 10], [30, 20, 5]]},
    MultiPoint5 = {'MultiPoint', [[20], [30]]},
    [{"Valid MultiPoint (a)",
      ?_assert(is_valid_geometry(MultiPoint1))},
     {"Valid MultiPoint (b)",
      ?_assert(is_valid_geometry(MultiPoint2))},
     {"Valid MultiPoint (c)",
      ?_assert(is_valid_geometry(MultiPoint3))},
     {"Invalid MultiPoint: too many coords",
      ?_assertMatch({false, _}, is_valid_geometry(MultiPoint4))},
     {"Invalid MultiPoint: not enough coords",
      ?_assertMatch({false, _}, is_valid_geometry(MultiPoint5))}].

is_valid_geometry_multi_line_test_() ->
    % Tests for MultiLineString geometries (basically the same as for polygons
    MLS1 = {'MultiLineString', [[[20, 30], [50, 60], [30, 10], [20, 30]]]},
    MLS2 = {'MultiLineString', [[[20, 30], [50, 60], [10, 70], [20, 30]],
        [[20, 55], [28, 60], [33, 53], [25, 48], [20, 55]]]},
    MLS3 = {'MultiLineString', [[20, 30], [50, 60], [30, 10], [20, 30]]},
    MLS4 = {'MultiLineString', [[[20, 30], [50, 60], [30], [20, 30]]]},
    [{"Valid MultiLineString (a)",
      ?_assert(is_valid_geometry(MLS1))},
     {"Valid MultiLineString (b)",
      ?_assert(is_valid_geometry(MLS2))},
     {"Invalid MultiLineString: not nested enough",
      ?_assertMatch({false, _}, is_valid_geometry(MLS3))},
     {"Invalid MultiLineString: invalid point somewhere",
      ?_assertMatch({false, _}, is_valid_geometry(MLS4))}].

is_valid_geometry_multi_polygon_test_() ->
    % Tests for MultiPolygons
    MPY1 = {'MultiPolygon', [[[[20, 30], [50, 60], [30, 10], [20, 30]]],
        [[[120, 130], [150, 160], [110, 170], [120, 130]]]]},
    MPY2 = {'MultiPolygon', [[[[20, 30], [50, 60], [10, 70], [20, 30]],
        [[20, 55], [28, 60], [33, 53], [25, 48], [20, 55]]],
        [[[120, 130], [150, 160], [130, 110], [120, 130]]]]},
    MPY3 = {'MultiPolygon', [[20, 30], [50, 60], [30, 10], [20, 30]]},
    MPY4 = {'MultiPolygon', [[[20, 30], [50, 60], [30, 10], [20, 30]]]},
    MPY5 = {'MultiPolygon', [[[[20, 30], [50, 60], [30, 10], [20, 30]]],
        [[[120, 130], [150, 160], [110], [120, 130]]]]},
    MPY6 = {'MultiPolygon', [[[[20, 30], [50, 60], [30, 10, 93], [20, 30]]],
        [[[120, 130], [150, 160], [110,170], [120, 130]]]]},
    [{"Valid MultiPolygon",
      ?_assert(is_valid_geometry(MPY1))},
     {"Valid MultiPolygon with hole",
      ?_assert(is_valid_geometry(MPY2))},
     {"Invalid MultiPolygon: not nested enough (a)",
      ?_assertMatch({false, _}, is_valid_geometry(MPY3))},
     {"Invalid MultiPolygon: not nested enough (b)",
      ?_assertMatch({false, _}, is_valid_geometry(MPY4))},
     {"Invalid MultiLineString: invalid point somewhere (a)",
      ?_assertMatch({false, _}, is_valid_geometry(MPY5))},
     {"Invalid MultiLineString: invalid point somewhere (b)",
      ?_assertMatch({false, _}, is_valid_geometry(MPY6))}].

is_valid_geometry_multi_collection_test_() ->
    % Tests for GeometryCollection
    GC1 = {'GeometryCollection', [
        {'Point', [20, 30]},
        {'LineString', [[20, 30], [50, 60]]},
        {'Polygon', [[[20, 30], [50, 60], [10, 70], [20, 30]],
                [[20, 55], [28, 60], [33, 53], [25, 48], [20, 55]]]},
        {'MultiPoint', [[20, 30], [50, 60]]}
    ]},
    GC2 = {'GeometryCollection', [
        {'Point', [20, 30]},
        {'LineString', [[20, 30], [50, 60]]},
        {'Polygon', [[[20, 30], [50, 60], [10, 70], [20, 30]],
                [[20, 55], [28, 60], [33], [25, 48], [20, 55]]]},
        {'MultiPoint', [[20, 30], [50, 60]]}
    ]},
    GC3 = {'GeometryCollection', [
        {'Something', [[20, 30], [50, 60]]}
    ]},
    [{"Valid GeometryCollection",
      ?_assert(is_valid_geometry(GC1))},
     {"Invalid GeometryCollection: invalid point somewhere",
      ?_assertMatch({false, _}, is_valid_geometry(GC2))},
     {"Invalid GeometryCollection: invalid geometry type",
      ?_assertMatch({false, _}, is_valid_geometry(GC3))}].

-endif.
