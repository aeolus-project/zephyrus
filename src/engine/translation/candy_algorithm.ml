(****************************************************************************)
(*                                                                          *)
(*    This file is part of Zephyrus.                                        *)
(*                                                                          *)
(*    Zephyrus is free software: you can redistribute it and/or modify      *)
(*    it under the terms of the GNU General Public License as published by  *)
(*    the Free Software Foundation, either version 3 of the License, or     *)
(*    (at your option) any later version.                                   *)
(*                                                                          *)
(*    Zephyrus is distributed in the hope that it will be useful,           *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*    GNU General Public License for more details.                          *)
(*                                                                          *)
(*    You should have received a copy of the GNU General Public License     *)
(*    along with Zephyrus.  If not, see <http://www.gnu.org/licenses/>.     *)
(*                                                                          *)
(****************************************************************************)

(** Component matching algorythm for creating bindings. *)

(** All that is extremely generic. Sorry... *) (* <- very nice comment Jakub :p *)

(** {2 Ordered types for [require_arity] and [provide_arity].} *)

type comparison = Lt | Eq | Gt
let compare_int x y =
    if x = y then Eq
    else (if x > y then Gt else Lt)

module type ORDERED_DECREMENTABLE_TYPE = sig
  type t
  val compare : t -> t -> comparison
  val is_zero : t -> bool
  val decrement : t -> t
end

module DecrementableNatural : ORDERED_DECREMENTABLE_TYPE = struct
  type t = int
    
  let compare = compare_int
  let is_zero x = (x = 0)
  let decrement x = (x - 1)
end

module DecrementableIntegerWithInfinity : ORDERED_DECREMENTABLE_TYPE = struct
  type t = 
  | FiniteInteger of int 
  | InfiniteInteger
    
  let compare x y = if x = y then Eq
    else (match (x,y) with
      | (FiniteInteger x, FiniteInteger y) -> compare_int x y
      | (InfiniteInteger, _ ) -> Gt
      | ( _, InfiniteInteger) -> Lt)
  let is_zero x = (x = (FiniteInteger 0))
  let decrement x = match x with
    | FiniteInteger x -> FiniteInteger (x - 1)
    | InfiniteInteger -> InfiniteInteger
end


(** {2 Generic requirer and provider types.} *)

module type REQUIRER_PROVIDER_TYPES = sig

  module Require_arity : ORDERED_DECREMENTABLE_TYPE
  module Provide_arity : ORDERED_DECREMENTABLE_TYPE

  type requirer_key_t
  type require_arity = Require_arity.t
  type provider_key_t
  type provide_arity = Provide_arity.t
  type result_t

  module Requirers : sig
    type t
    val iter : ( (requirer_key_t * require_arity) -> unit) -> t -> unit
  end

  module Providers : sig
    type t
    val is_empty  : t -> bool
    val max_value : t -> (provider_key_t * provide_arity)
    val remove    : provider_key_t -> t -> t
    val decrement : provider_key_t -> t -> t
  end

  module Results : sig (* TODO: bad. We could translate the result in bindings directly here. *)
    type t
    val empty : t
    val add   : requirer_key_t -> provider_key_t -> t -> t
  end
end


module type REQUIRER_PROVIDER_TYPE_PARAMETER =
  sig
    type requirer_key_t
    type provider_key_t 
  end




(** {2 Simple implementation of requirer and provider modules based on association lists.} *)

module List_requirer_provider_types =
  functor (Requirer_provider_type_parameter : REQUIRER_PROVIDER_TYPE_PARAMETER) ->
  functor (Require_arity : ORDERED_DECREMENTABLE_TYPE) ->
  functor (Provide_arity : ORDERED_DECREMENTABLE_TYPE) ->
  struct

    module Type_Parameter = Requirer_provider_type_parameter
    module Require_arity = Require_arity
    module Provide_arity = Provide_arity

    type requirer_key_t = Type_Parameter.requirer_key_t    
    type require_arity  = Require_arity.t
    type provider_key_t = Type_Parameter.provider_key_t
    type provide_arity  = Provide_arity.t

    type result_t = {
      requires : requirer_key_t;
      provides : provider_key_t;
    }

    module Requirers =
      struct

        type t = (requirer_key_t * require_arity) list

        let iter = List.iter

      end
    
    module Providers =
      struct
    
        type t = (provider_key_t * provide_arity) list
        
        let is_empty l = 
          ( (List.length l) = 0 )
        
        let max_value l =
          let rec max_value_helper l max =
            match l with
            | [] -> max
            | (key, value) :: t ->
                let max_value = snd max
                in
                if (Provide_arity.compare value max_value) = Gt
                then max_value_helper t (key, value)
                else max_value_helper t max
          in
          max_value_helper l (List.hd l)
    
        let remove key l =
          List.remove_assoc key l
    
        let decrement key l =
          List.map (fun (kkey, value) ->
            if kkey = key
            then (kkey, Provide_arity.decrement value)
            else (kkey, value)
          ) l
    
      end
    
    module Results =
      struct
    
        type t = result_t list
    
        let empty = []
    
        let add requires provides l =
          let result = {
            requires = requires;
            provides = provides;
          }
          in 
          (result :: l)
    
      end

end


(** {2 Better implementation of requirer and provider modules based on sets.} *)

module Set_map_requirer_provider_types =
  functor (Requirer_provider_type_parameter : REQUIRER_PROVIDER_TYPE_PARAMETER) ->
  functor (Require_arity : ORDERED_DECREMENTABLE_TYPE) ->
  functor (Provide_arity : ORDERED_DECREMENTABLE_TYPE) ->
  struct

    module Type_Parameter = Requirer_provider_type_parameter
    module Require_arity = Require_arity
    module Provide_arity = Provide_arity

    type requirer_key_t = Type_Parameter.requirer_key_t    
    type require_arity  = Require_arity.t
    type provider_key_t = Type_Parameter.provider_key_t
    type provide_arity  = Provide_arity.t

    type result_t = {
      requires : requirer_key_t;
      provides : provider_key_t;
    }

    module Requirers =
      struct

        type t = (requirer_key_t * require_arity) list

        let iter = List.iter

      end
    
    module Providers =
      struct
    
        open Data_common

        module Provider_key_map = Map.Make(struct type t = provider_key_t let compare = compare end)

        module Provider_key_provider_arity_set = Set.Make(struct 
          type t = (provider_key_t * provide_arity) 
          let compare (x_key, x_arity) (y_key, y_arity) = 
            match Provide_arity.compare x_arity y_arity with
            | Lt -> -1 
            | Eq -> compare x_key y_key
            | Gt ->  1
        end)

        type t = {
          map : provide_arity Provider_key_map.t;
          set : Provider_key_provider_arity_set.t;
        }
        
        let is_empty t = 
          ( Provider_key_provider_arity_set.is_empty t.set )
        
        let max_value t =
          Provider_key_provider_arity_set.max_elt t.set
    
        let remove key t =
          let arity = Provider_key_map.find key t.map in
          {
            map = Provider_key_map.remove key t.map;
            set = Provider_key_provider_arity_set.remove (key, arity) t.set;
          }
    
        let decrement key t =
          let arity = Provider_key_map.find key t.map in
          let decremented_arity = Provide_arity.decrement arity in
          {
            map = Provider_key_map.add key decremented_arity t.map;
            set = Provider_key_provider_arity_set.add (key, decremented_arity) (Provider_key_provider_arity_set.remove (key, arity) t.set);
          }
    
      end
    
    module Results =
      struct
    
        type t = result_t list
    
        let empty = []
    
        let add requires provides l =
          let result = {
            requires = requires;
            provides = provides;
          }
          in 
          (result :: l)
    
      end

end


(** {2 Generic matching algorithm implementation.} *)

module type MATCH_REQUIRERS_WITH_PROVIDERS =
  functor (Requirer_provider_types : REQUIRER_PROVIDER_TYPES) ->
  sig
    val matching_algorithm : 
      Requirer_provider_types.Requirers.t -> 
      Requirer_provider_types.Providers.t -> 
      Requirer_provider_types.Results.t option
  end


module Match_requirers_with_providers : MATCH_REQUIRERS_WITH_PROVIDERS =
  functor (Requirer_provider_types : REQUIRER_PROVIDER_TYPES) ->
  struct

    open Requirer_provider_types
    (* Gives direct access to modules:
       + Requirers
       + Providers
       + Results 

       + Require_arity
       + Provide_arity
    *)

    (* Exception raised when finding a correct matching is impossible. *)
    exception Impossible

    let matching_algorithm requirers providers =
      try  
        (* The "result" variable accumulates the bindings between providers and requirers. *)
        let result = ref Results.empty
  
        (* The "providers" variable holds the current information about available providers and their capacity. *)
        and providers = ref providers
        in
  
        (* We need to bind each requirer to a number of different providers that he requies. *)
        Requirers.iter ( fun (requirer_key, requirer_value) ->
          (* "requirer_key"   is the name of the requirer. *)
          (* "requirer_value" is the number of different providers this requirer requires. *)
  
          (* The "providers_lefts" variable holds all the providers that this requirer is not bound to yet. 
             With each step it will become smaller, as our requirer will be bound to another provider. *)
          let providers_left = ref !providers in
  
          (* We repeat the process of binding our requirer to a different provider as many times as it requires.  *)
          let k = ref requirer_value in
          while not (Require_arity.is_zero !k) do
  
            k := Require_arity.decrement !k;

            (* If there are no more providers not bound to our requirer left, we have lost. *)
            if Providers.is_empty !providers_left
            then raise Impossible
  
            else
  
              (* We take the provider which has the most ports left unbound (and which is not bound to our requirer). *)
              let (provider_key, provider_value) =
                Providers.max_value !providers_left
              in
  
              (* If the best provider available has no more ports left unbound, then we have lost. *)
              if Provide_arity.is_zero provider_value
              then raise Impossible
  
              (* Else we can bind the requirer to the provider and adjust the other variables accordingly. *)
              else
  
                (* We add this requirer-provider binding to the results. *)
                result         := Results.add          requirer_key provider_key !result;
  
                (* This provider is now bound to current requirer, so it becomes unavailable, as all
                   the providers bound to a given requirer must be different. *)
                providers_left := Providers.remove     provider_key !providers_left;
  
                (* This provider has one more unbound port left for binding. *)
                providers      := Providers.decrement  provider_key !providers;
          done;
        ) requirers;
        
        (* As we have bound all the ports of all the requirers, we have won! Congratulations! *)
        (Some !result)
  
      (* We have encountered an unsolvable situation somewhere during the algorithm. Thus, it is not possible 
         tho correctly satisfy the requirements of all the requirers with the available providers. *)
      with
      | Impossible -> None

  end




(** {2 Putting it all together into usable modules.} *)

(** {3 For [string] keys.} *)

module Requirer_provider_type_param_string =
  struct 
      type key_t = string
      type requirer_key_t = key_t
      type provider_key_t = key_t
  end

module String_list_requirer_provider_types =
  List_requirer_provider_types(Requirer_provider_type_param_string)(DecrementableNatural)(DecrementableIntegerWithInfinity)

module String_list_match_requirers_with_providers =
  Match_requirers_with_providers(String_list_requirer_provider_types)


(** {3 For [int] keys.} *)

module Requirer_provider_type_param_int =
  struct 
      type key_t = int
      type requirer_key_t = key_t
      type provider_key_t = key_t
  end  

module Int_list_requirer_provider_types =
  List_requirer_provider_types(Requirer_provider_type_param_int)(DecrementableNatural)(DecrementableIntegerWithInfinity)  

module Int_list_match_requirers_with_providers =
  Match_requirers_with_providers(Int_list_requirer_provider_types)


module Int_set_map_requirer_provider_types =
  Set_map_requirer_provider_types(Requirer_provider_type_param_int)(DecrementableNatural)(DecrementableIntegerWithInfinity)  

module Int_set_map_match_requirers_with_providers =
  Match_requirers_with_providers(Int_set_map_requirer_provider_types)




