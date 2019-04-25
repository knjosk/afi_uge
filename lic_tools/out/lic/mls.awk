#BEGIN { RS="------------------------"; FS ="\n"}
BEGIN { RS="admin > glu" ; FS ="\n"}
{
	#if (FNR==3 )
	if (FNR==2 )
      for ( i=2; i < NF ; i++)
          print $i
}
