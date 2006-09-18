/* smlsqlite - a binding of SQLite for Standard ML.                    */
/*                                                                     */
/* Copyright (c) 2005, 2006 Henning Niss and Ken Friis Larsen.         */
/* All rights reserved. See the file LICENSE for license information.  */

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <sqlite3.h>

/* Mosml stuff */
#include <mlvalues.h> 
#include <fail.h>
#include <alloc.h>
#include <memory.h>
#include <callback.h>
#include <str.h>


#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#else
#define EXTERNML
#endif

#define Some(opt) (Field(opt, 0))


/* SML lists - mostly taken from mGTK */
#define IsCons(x) (Tag_val(x) != 0)  
#define Nil_list  (Atom(0))
#define Head(xs)  (Field(xs, 0))
#define Tail(xs)  (Field(xs, 1))

static inline char* copy_sml_string(value s) {
  mlsize_t len = string_length( s );
  char* mlstr  = String_val( s );
  char* result = malloc( len+1 );
  if( result == NULL ) {
    failwith("copy_sml_string: out of memory");
  }
  
  return strncpy(result, mlstr, len+1);
}

static inline value make_string_option(char* str) {
  if(str == NULL) return NONE;
  else {
    value result;
    Push_roots(tmp, 1);
      tmp[0] = copy_string( str );
      result = alloc(1, SOMEtag);
      Some(result) = tmp[0];
    Pop_roots(); 
    return result;
  }
}

static inline value make_cons(value elem, value tail) {
  value result;
  Push_roots(tmp, 2);
    tmp[0] = elem;
    tmp[1] = tail;
    result       = alloc(2, 1);     /* Allocate a cons cell, tag == 1 */
    Head(result) = tmp[0];          /* result is just allocated, thus */
    Tail(result) = tmp[1];          /* we don't need to use modify    */
  Pop_roots();
  return result;
}

static inline value string_option_array_to_list(int n, char** arr) {
  value result;
  Push_roots(tmp, 1);
    tmp[0] = Nil_list;
    for( ; n > 0; n--) {
      value ml_str = make_string_option( arr[n-1] );
      tmp[0] = make_cons( ml_str, tmp[0] );
    }
    result = tmp[0];  
  Pop_roots();
  return result;
}    

static inline value string_array_to_list(int n, char** arr) {
  value result;
  Push_roots(tmp, 1);
    tmp[0] = Nil_list;
    for( ; n > 0; n--) {
      assert(arr[n-1] != NULL);
      value ml_str = copy_string( arr[n-1] );
      tmp[0] = make_cons( ml_str, tmp[0] );
    }
    result = tmp[0];  
  Pop_roots();
  return result;
}    

/* callback stuff - mostly taken from mGTK */
static int msqlite_callback_dispatch (void* data, int argc,
				      char **argv, char **columnNames) {
  value val;
  valueptr mvp;

  Push_roots(r, 3);  
    r[0] = string_option_array_to_list( argc, argv );
    r[1] = string_array_to_list( argc, columnNames );
    r[2] = alloc_tuple( 2 );
    Field(r[2], 0) = r[0];
    Field(r[2], 1) = r[1];
    mvp = get_valueptr("msqlite_callback_dispatch"); // may allocate!
    val = r[2];
  Pop_roots();

  if (mvp == (valueptr) NULL) {
    failwith("Cannot find msqlite_callback_dispatch");
  }

  val = callbackptr2( mvp, (value) data, val );
  return !( Bool_val(val) );
}


/*
static void msqlite_callback_destroy (void* data) {
  valueptr mvp = get_valueptr("msqlite_callback_destroy"); 
  if(mvp == (valueptr) NULL)
    failwith("Cannot find msqlite_callback_destroy");

  callbackptr(mvp, (value) data);
}
*/


/* sqlite proper */


#define Db_val(x) (((sqlite3 **) (x)) [0])  // Also an l-value

static inline value make_sqlite_db (sqlite3* obj) { 
  value res; 
  res = alloc( 1, Abstract_tag );
  Db_val(res) = obj;  
  return res; 
}

#define Stmt_val(x) (((sqlite3_stmt **) (x)) [1]) // Also an l-value

static void ml_finalize_stmt (value val) {
  sqlite3_stmt* stmt = Stmt_val(val);
  if(stmt != NULL) sqlite3_finalize(stmt);
}

static inline value make_sqlite_stmt (sqlite3_stmt* obj) { 
  value res; 
  res = alloc_final ( 2, ml_finalize_stmt, 0, 1 );
  Stmt_val(res) = obj;  
  return res; 
}


static inline value Val_sqlite_status(int status) {
  // Moscow ML black magic.  Moscow ML sorts the constructors by name 
  // before assigning them a number.
  switch ( status ) {
  case SQLITE_ABORT:      return Atom(  0 );
  case SQLITE_AUTH:       return Atom(  1 ); 
  case SQLITE_BUSY:       return Atom(  2 );
  case SQLITE_CANTOPEN:   return Atom(  3 ); 
  case SQLITE_CONSTRAINT: return Atom(  4 ); 
  case SQLITE_CORRUPT:    return Atom(  5 ); 
  case SQLITE_DONE:       return Atom(  6 );
  case SQLITE_EMPTY:      return Atom(  7 ); 
  case SQLITE_ERROR:      return Atom(  8 );
  case SQLITE_FORMAT:     return Atom(  9 );
  case SQLITE_FULL:       return Atom( 10 ); 
  case SQLITE_INTERNAL:   return Atom( 11 );
  case SQLITE_INTERRUPT:  return Atom( 12 ); 
  case SQLITE_IOERR:      return Atom( 13 ); 
  case SQLITE_LOCKED:     return Atom( 14 ); 
  case SQLITE_MISMATCH:   return Atom( 15 ); 
  case SQLITE_MISUSE:     return Atom( 16 ); 
  case SQLITE_NOLFS:      return Atom( 17 ); 
  case SQLITE_NOMEM:      return Atom( 18 ); 
  case SQLITE_NOTADB:     return Atom( 19 );
  case SQLITE_NOTFOUND:   return Atom( 20 ); 
  case SQLITE_OK:         return Atom( 21 );
  case SQLITE_PERM:       return Atom( 22 );
  case SQLITE_PROTOCOL:   return Atom( 23 ); 
  case SQLITE_RANGE:      return Atom( 24 );
  case SQLITE_READONLY:   return Atom( 25 ); 
  case SQLITE_ROW:        return Atom( 26 ); 
  case SQLITE_SCHEMA:     return Atom( 27 ); 
  case SQLITE_TOOBIG:     return Atom( 28 );
  // Default should never happen
  default:                return Val_sqlite_status( SQLITE_ERROR );
  }
}

static inline value Val_sqlite_column_type(int column_type) {
  // Moscow ML black magic again.  Constructor are sorted by name 
  // before they are assigned number
  switch( column_type ){
  case SQLITE_BLOB:    return Atom( 0 );
  case SQLITE_FLOAT:   return Atom( 1 );
  case SQLITE_INTEGER: return Atom( 2 );
  case SQLITE_NULL:    return Atom( 3 );
  case SQLITE_TEXT:    return Atom( 4 );
  // Default should never happen
  default:             return Atom( 0 );
  }
}

EXTERNML value msqlite_open(value file) { /* ML */
  value pair;
  sqlite3 *db;
  int ret = sqlite3_open(String_val(file), &db);

  Push_roots(r, 1);  
    r[0] = make_sqlite_db( db );
    pair = alloc_tuple( 2 );
    Field(pair, 0) = Val_sqlite_status( ret );
    Field(pair, 1) = r[0];
  Pop_roots();

  return pair;
}

EXTERNML value msqlite_close(value db) { /* ML */
  return Val_sqlite_status(sqlite3_close(Db_val(db)));
}

EXTERNML value msqlite_errcode(value db) { /* ML */
  return Val_sqlite_status(sqlite3_errcode(Db_val(db)));
}

EXTERNML value msqlite_errmsg(value db) { /* ML */
  return copy_string((char*)sqlite3_errmsg(Db_val(db)));
}

EXTERNML value msqlite_exec(value db, value sql, value callback) { /* ML */
  void * data;
  sqlite3_callback dispatch;
  if ( callback == NONE ) {
    data = 0;
    dispatch = NULL;
  } else {
    data = (void*) Some(callback);
    dispatch = msqlite_callback_dispatch;
  }
    
  char* s = copy_sml_string( sql );
  int status = sqlite3_exec( Db_val(db), 
                             s,
			     dispatch,
                             data, 
			     NULL );
  free( s );
  return Val_sqlite_status( status );
}

EXTERNML value msqlite_prepare(value db, value sql) { /* ML */
  value tup;
  sqlite3_stmt* stmt;
  const char* remainder;
  char* s = copy_sml_string(sql);
  // status ignored here, but explicitly checked on the ML side
  sqlite3_prepare(Db_val(db), s, -1, &stmt, &remainder);

  Push_roots(tmp, 2);
    tmp[0] = make_sqlite_stmt(stmt);
    tmp[1] = make_string_option((char*)remainder); // FIXME: return NONE on len==0?
    tup = alloc_tuple(2);
    Field(tup, 0) = tmp[0];
    Field(tup, 1) = tmp[1];
  Pop_roots();
  
  free(s);
  return tup;
}

EXTERNML value msqlite_finalize(value stmt) { /* ML */
  int status;
  sqlite3_stmt* s = Stmt_val(stmt);
  if(s != NULL) {
    status = sqlite3_finalize(s);
    Stmt_val(stmt) = NULL;  
  } else { // already finalized by mosml
    status = SQLITE_OK;
  }
  return Val_sqlite_status(status);
}

EXTERNML value msqlite_reset(value stmt) { /* ML */
  return Val_sqlite_status(sqlite3_reset(Stmt_val(stmt)));
}

EXTERNML value msqlite_step(value stmt) { /* ML */
  return Val_sqlite_status(sqlite3_step(Stmt_val(stmt)));
}

EXTERNML value msqlite_data_count(value stmt) { /* ML */
  return Val_long(sqlite3_data_count(Stmt_val(stmt)));
}

EXTERNML value msqlite_column_count(value stmt) { /* ML */
  return Val_long(sqlite3_column_count(Stmt_val(stmt)));
}

EXTERNML value msqlite_column_text(value stmt, value col) { /* ML */
  return copy_string((char*)sqlite3_column_text(Stmt_val(stmt), Long_val(col)));
}

EXTERNML value msqlite_column_name(value stmt, value col) { /* ML */
  return copy_string((char*)sqlite3_column_name(Stmt_val(stmt), Long_val(col)));
}

EXTERNML value msqlite_column_decltype(value stmt, value col) { /* ML */
  return make_string_option((char*)sqlite3_column_decltype(Stmt_val(stmt), Long_val(col)));
}

EXTERNML value msqlite_column_type(value stmt, value col) { /* ML */
  return Val_sqlite_column_type(sqlite3_column_type(Stmt_val(stmt), Long_val(col)));
}

EXTERNML value msqlite_column_int(value stmt, value col) { /* ML */
  return Val_long(sqlite3_column_int(Stmt_val(stmt), Long_val(col)));
}

EXTERNML value msqlite_column_double(value stmt, value col) { /* ML */
  return copy_double(sqlite3_column_double(Stmt_val(stmt), Long_val(col)));
}

#define Val_word8(c) (Val_long((long) c))
static value byte_array_to_vector(const char *arr, int n) {
  value result;
  int i;
  if(n <= 0) 
    result = Atom (0);
  else {
    Push_roots(tmp,1);
    tmp[0] = n < Max_young_wosize ? alloc(n, 0) : alloc_shr(n,0);
    /* initializing not necessary since we don't allocate */
    for(i = 0; i < n; i++) {
      Field(tmp[0], i) = Val_word8(arr[i]);
    }
    result = tmp[0];
    Pop_roots();
  }
  return result;
}

EXTERNML value msqlite_column_blob(value stmt, value col) { /* ML */
  value result;
  int c = Long_val(col);
  sqlite3_stmt* s = Stmt_val(stmt);
  int n = sqlite3_column_bytes(s,c);
  const char *arr = sqlite3_column_blob(s,c);
  result = byte_array_to_vector(arr, n);
  return result;
}
