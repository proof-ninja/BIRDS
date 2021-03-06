/*_____get datalog program_______
?- tracks3(TRACK,RATING,ALBUM,QUANTITY).

tracks3(TRACKS3_A4_TRACK,TRACKS3_A4_RATING,TRACKS3_A4_ALBUM,TRACKS3_A4_QUANTITY) :- tracks3_med(TRACKS3_A4_TRACK,TRACKS3_A4_RATING,TRACKS3_A4_ALBUM,TRACKS3_A4_QUANTITY) , not __dummy__delta__insert__tracks2(TRACKS3_A4_TRACK,TRACKS3_A4_RATING,TRACKS3_A4_ALBUM,TRACKS3_A4_QUANTITY).

__dummy__delta__insert__tracks2(TRACK,RATING,ALBUM,QUANTITY) :- tracks3_med(TRACK,RATING,ALBUM,QUANTITY) , not tracks2(TRACK,RATING,ALBUM,QUANTITY) , QUANTITY > 2.

tracks3_med(TRACK,RATING,ALBUM,QUANTITY) :- tracks2(TRACK,RATING,ALBUM,QUANTITY) , QUANTITY > 2.

______________*/

CREATE OR REPLACE VIEW public.tracks3 AS SELECT __dummy__.col0 AS TRACK,__dummy__.col1 AS RATING,__dummy__.col2 AS ALBUM,__dummy__.col3 AS QUANTITY FROM (SELECT tracks3_a4_0.col0 AS col0, tracks3_a4_0.col1 AS col1, tracks3_a4_0.col2 AS col2, tracks3_a4_0.col3 AS col3 FROM (SELECT tracks3_med_a4_0.col0 AS col0, tracks3_med_a4_0.col1 AS col1, tracks3_med_a4_0.col2 AS col2, tracks3_med_a4_0.col3 AS col3 FROM (SELECT tracks2_a4_0.TRACK AS col0, tracks2_a4_0.RATING AS col1, tracks2_a4_0.ALBUM AS col2, tracks2_a4_0.QUANTITY AS col3 FROM public.tracks2 AS tracks2_a4_0 WHERE tracks2_a4_0.QUANTITY  >  2 ) AS tracks3_med_a4_0 WHERE NOT EXISTS ( SELECT * FROM (SELECT tracks3_med_a4_0.col0 AS col0, tracks3_med_a4_0.col1 AS col1, tracks3_med_a4_0.col2 AS col2, tracks3_med_a4_0.col3 AS col3 FROM (SELECT tracks2_a4_0.TRACK AS col0, tracks2_a4_0.RATING AS col1, tracks2_a4_0.ALBUM AS col2, tracks2_a4_0.QUANTITY AS col3 FROM public.tracks2 AS tracks2_a4_0 WHERE tracks2_a4_0.QUANTITY  >  2 ) AS tracks3_med_a4_0 WHERE tracks3_med_a4_0.col3  >  2 AND NOT EXISTS ( SELECT * FROM public.tracks2 AS tracks2_a4 WHERE tracks2_a4.QUANTITY IS NOT DISTINCT FROM tracks3_med_a4_0.col3 AND tracks2_a4.ALBUM IS NOT DISTINCT FROM tracks3_med_a4_0.col2 AND tracks2_a4.RATING IS NOT DISTINCT FROM tracks3_med_a4_0.col1 AND tracks2_a4.TRACK IS NOT DISTINCT FROM tracks3_med_a4_0.col0 ) ) AS __dummy__delta__insert__tracks2_a4 WHERE __dummy__delta__insert__tracks2_a4.col3 IS NOT DISTINCT FROM tracks3_med_a4_0.col3 AND __dummy__delta__insert__tracks2_a4.col2 IS NOT DISTINCT FROM tracks3_med_a4_0.col2 AND __dummy__delta__insert__tracks2_a4.col1 IS NOT DISTINCT FROM tracks3_med_a4_0.col1 AND __dummy__delta__insert__tracks2_a4.col0 IS NOT DISTINCT FROM tracks3_med_a4_0.col0 ) ) AS tracks3_a4_0  ) AS __dummy__;

CREATE OR REPLACE FUNCTION public.tracks3_procedure()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
  DECLARE
  text_var1 text;
  text_var2 text;
  text_var3 text;
  temprec record;
  BEGIN
    CREATE TEMPORARY TABLE __temp__tracks3 WITH OIDS ON COMMIT DROP AS SELECT * FROM public.tracks3;
    IF TG_OP = 'INSERT' THEN
      INSERT INTO __temp__tracks3 SELECT (NEW).*; 
    ELSIF TG_OP = 'UPDATE' THEN
      DELETE FROM __temp__tracks3 WHERE (TRACK,RATING,ALBUM,QUANTITY) = OLD;
      INSERT INTO __temp__tracks3 SELECT (NEW).*; 
    ELSIF TG_OP = 'DELETE' THEN
      DELETE FROM __temp__tracks3 WHERE (TRACK,RATING,ALBUM,QUANTITY) = OLD;
    END IF;
    CREATE TEMPORARY TABLE __dummy__delta__delete__tracks2 WITH OIDS ON COMMIT DROP AS SELECT __dummy__delta__delete__tracks2_a4_0.col0 AS col0, __dummy__delta__delete__tracks2_a4_0.col1 AS col1, __dummy__delta__delete__tracks2_a4_0.col2 AS col2, __dummy__delta__delete__tracks2_a4_0.col3 AS col3 FROM (SELECT tracks2_a4_0.TRACK AS col0, tracks2_a4_0.RATING AS col1, tracks2_a4_0.ALBUM AS col2, tracks2_a4_0.QUANTITY AS col3 FROM public.tracks2 AS tracks2_a4_0 WHERE tracks2_a4_0.QUANTITY  >  2 AND NOT EXISTS ( SELECT * FROM (SELECT __temp__tracks3_a4_0.TRACK AS col0, __temp__tracks3_a4_0.RATING AS col1, __temp__tracks3_a4_0.ALBUM AS col2, __temp__tracks3_a4_0.QUANTITY AS col3 FROM __temp__tracks3 AS __temp__tracks3_a4_0  ) AS tracks3_a4 WHERE tracks3_a4.col3 IS NOT DISTINCT FROM tracks2_a4_0.QUANTITY AND tracks3_a4.col2 IS NOT DISTINCT FROM tracks2_a4_0.ALBUM AND tracks3_a4.col1 IS NOT DISTINCT FROM tracks2_a4_0.RATING AND tracks3_a4.col0 IS NOT DISTINCT FROM tracks2_a4_0.TRACK ) ) AS __dummy__delta__delete__tracks2_a4_0  ;
CREATE TEMPORARY TABLE __dummy__delta__insert__tracks2 WITH OIDS ON COMMIT DROP AS SELECT __dummy__delta__insert__tracks2_a4_0.col0 AS col0, __dummy__delta__insert__tracks2_a4_0.col1 AS col1, __dummy__delta__insert__tracks2_a4_0.col2 AS col2, __dummy__delta__insert__tracks2_a4_0.col3 AS col3 FROM (SELECT tracks3_a4_0.col0 AS col0, tracks3_a4_0.col1 AS col1, tracks3_a4_0.col2 AS col2, tracks3_a4_0.col3 AS col3 FROM (SELECT __temp__tracks3_a4_0.TRACK AS col0, __temp__tracks3_a4_0.RATING AS col1, __temp__tracks3_a4_0.ALBUM AS col2, __temp__tracks3_a4_0.QUANTITY AS col3 FROM __temp__tracks3 AS __temp__tracks3_a4_0  ) AS tracks3_a4_0 WHERE tracks3_a4_0.col3  >  2 AND NOT EXISTS ( SELECT * FROM public.tracks2 AS tracks2_a4 WHERE tracks2_a4.QUANTITY IS NOT DISTINCT FROM tracks3_a4_0.col3 AND tracks2_a4.ALBUM IS NOT DISTINCT FROM tracks3_a4_0.col2 AND tracks2_a4.RATING IS NOT DISTINCT FROM tracks3_a4_0.col1 AND tracks2_a4.TRACK IS NOT DISTINCT FROM tracks3_a4_0.col0 ) ) AS __dummy__delta__insert__tracks2_a4_0  ; 
 FOR temprec IN ( SELECT * FROM __dummy__delta__delete__tracks2) LOOP 
        DELETE FROM public.tracks2 WHERE (TRACK,RATING,ALBUM,QUANTITY) IS NOT DISTINCT FROM  (temprec.col0,temprec.col1,temprec.col2,temprec.col3);
        END LOOP;
INSERT INTO public.tracks2 SELECT * FROM __dummy__delta__insert__tracks2;
    RETURN NULL;
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
        RAISE object_not_in_prerequisite_state USING MESSAGE = 'no permission to insert or update or delete from public.tracks3';
    WHEN OTHERS THEN
        GET STACKED DIAGNOSTICS text_var1 = RETURNED_SQLSTATE,
                                text_var2 = PG_EXCEPTION_DETAIL,
                                text_var3 = MESSAGE_TEXT;
        RAISE SQLSTATE 'DA000' USING MESSAGE = 'error on the trigger of public.tracks3 ; error code: ' || text_var1 || ' ; ' || text_var2 ||' ; ' || text_var3;
        RETURN NULL;
  END;
  
$$;
DROP TRIGGER IF EXISTS tracks3_trigger ON public.tracks3;
CREATE TRIGGER tracks3_trigger
    INSTEAD OF INSERT OR UPDATE OR DELETE ON
      public.tracks3 FOR EACH ROW EXECUTE PROCEDURE public.tracks3_procedure();

