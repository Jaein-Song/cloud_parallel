subroutine ncwrite
    use varmod
    use netcdf
    !NC FILE OUT=====================================================================
print*, odir
status = nf90_create(odir,nf90_clobber,ncid);             call chkerr(status,'writing file')

status = nf90_def_dim(ncid,nam_tim,nt,dim_tim);              call chkerr(status,'dimensioning')
status = nf90_def_dim(ncid,nam_hgt,nh,dim_hgt);              call chkerr(status,'dimensioning')
status = nf90_put_att(ncid,vid_tims,att_unt,unt_tim);      call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_hgts,att_unt,unt_hgt);      call chkerr(status,'unit')
dims=(/dim_tim,dim_hgt/)
status = nf90_def_var(ncid,nam_bsc,nf90_float,dims,vid_bsc);   call chkerr(status,'defining')
status = nf90_def_var(ncid,nam_tim,nf90_float,dim_tim,vid_tim);   call chkerr(status,'defining')
status = nf90_def_var(ncid,nam_hgt,nf90_float,dim_hgt,vid_hgt);   call chkerr(status,'defining')
status = nf90_def_var(ncid,nam_cbh1,nf90_float,dim_tim,vid_cbh1);   call chkerr(status,'defining')
status = nf90_def_var(ncid,nam_cbh2,nf90_float,dim_tim,vid_cbh2);   call chkerr(status,'defining')
status = nf90_def_var(ncid,nam_cbh3,nf90_float,dim_tim,vid_cbh3);   call chkerr(status,'defining')
status = nf90_put_att(ncid,vid_tim,att_unt,unt_tim);      call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_hgt,att_unt,unt_hgt);      call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_cbh1,att_unt,unt_hgt);      call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_cbh2,att_unt,unt_hgt);      call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_cbh3,att_unt,unt_hgt);      call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_cbh1,'_FillValue',fv);      call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_cbh2,'_FillValue',fv);      call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_cbh3,'_FillValue',fv);      call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_bsc,att_unt,unt_bsc);      call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_bsc,'_FillValue',fv);      call chkerr(status,'Fillvalue')
status = nf90_enddef(ncid)
status = nf90_put_var(ncid,vid_tim,t);                    call chkerr(status,'t in')
status = nf90_put_var(ncid,vid_hgt,h);                    call chkerr(status,'h in')
status = nf90_put_var(ncid,vid_bsc,bscd);                              call chkerr(status,'bsc in')
status = nf90_put_var(ncid,vid_cbh1,cbh1o);                    call chkerr(status,'cbh1 in')
status = nf90_put_var(ncid,vid_cbh2,cbh2o);                    call chkerr(status,'cbh2 in')
status = nf90_put_var(ncid,vid_cbh3,cbh3o);                    call chkerr(status,'cbh3 in')
status = nf90_close(ncid);                                call chkerr(status,'end')



status = nf90_create(odir2,nf90_clobber,ncid);             call chkerr(status,'writing file')

status = nf90_def_dim(ncid,nam_tims,nts,dim_tims);           call chkerr(status,'dimensioning')
status = nf90_def_dim(ncid,nam_hgts,nhs,dim_hgts);           call chkerr(status,'dimensioning')
dimss=(/dim_tims,dim_hgts/)
status = nf90_def_var(ncid,nam_tims,nf90_float,dim_tims,vid_tims);     call chkerr(status,'01defining')
status = nf90_def_var(ncid,nam_hgts,nf90_float,dim_hgts,vid_hgts);     call chkerr(status,'02defining')
status = nf90_def_var(ncid,nam_cbct,nf90_float,dim_tims,vid_cbct);     call chkerr(status,'03defining')
status = nf90_def_var(ncid,nam_cbcv1,nf90_float,dim_tims,vid_cbcv1);   call chkerr(status,'04defining')
status = nf90_def_var(ncid,nam_cbcv2,nf90_float,dim_tims,vid_cbcv2);   call chkerr(status,'05defining')
status = nf90_def_var(ncid,nam_cbcv3,nf90_float,dim_tims,vid_cbcv3);   call chkerr(status,'06defining')
status = nf90_def_var(ncid,nam_bhct,nf90_float,dim_tims,vid_bhct);     call chkerr(status,'07defining')
status = nf90_def_var(ncid,nam_bhcv1,nf90_float,dim_tims,vid_bhcv1);   call chkerr(status,'08defining')
status = nf90_def_var(ncid,nam_bhcv2,nf90_float,dim_tims,vid_bhcv2);   call chkerr(status,'09defining')
status = nf90_def_var(ncid,nam_bhcv3,nf90_float,dim_tims,vid_bhcv3);   call chkerr(status,'10defining')
status = nf90_def_var(ncid,nam_bicv1,nf90_float,dim_tims,vid_bicv1);   call chkerr(status,'11defining')
status = nf90_def_var(ncid,nam_bicv2,nf90_float,dim_tims,vid_bicv2);   call chkerr(status,'12defining')
status = nf90_def_var(ncid,nam_bicv3,nf90_float,dim_tims,vid_bicv3);   call chkerr(status,'13defining')
status = nf90_def_var(ncid,nam_cbh1s,nf90_float,dim_tims,vid_cbh1s);   call chkerr(status,'14defining')
status = nf90_def_var(ncid,nam_cbh2s,nf90_float,dim_tims,vid_cbh2s);   call chkerr(status,'15defining')
status = nf90_def_var(ncid,nam_cbh3s,nf90_float,dim_tims,vid_cbh3s);   call chkerr(status,'16defining')
status = nf90_def_var(ncid,nam_blh1s,nf90_float,dim_tims,vid_blh1s);   call chkerr(status,'defining')
status = nf90_def_var(ncid,nam_blh2s,nf90_float,dim_tims,vid_blh2s);   call chkerr(status,'defining')
status = nf90_def_var(ncid,nam_blh3s,nf90_float,dim_tims,vid_blh3s);   call chkerr(status,'defining')
status = nf90_def_var(ncid,nam_bli1s,nf90_float,dim_tims,vid_bli1s);   call chkerr(status,'defining')
status = nf90_def_var(ncid,nam_bli2s,nf90_float,dim_tims,vid_bli2s);   call chkerr(status,'defining')
status = nf90_def_var(ncid,nam_bli3s,nf90_float,dim_tims,vid_bli3s);   call chkerr(status,'defining')
status = nf90_def_var(ncid,nam_bscs,nf90_float,dimss,vid_bscs);        call chkerr(status,'defining')
status = nf90_def_var(ncid,nam_bsct,nf90_float,dimss,vid_bsct);        call chkerr(status,'defining')
status = nf90_def_var(ncid,nam_bscv,nf90_float,dimss,vid_bscv);        call chkerr(status,'defining')
status = nf90_put_att(ncid,vid_cbct,'_FillValue',fv);                  call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_cbcv1,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_cbcv1,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_cbcv1,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_bhct,'_FillValue',fv);                  call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_bhcv1,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_bhcv1,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_bhcv1,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_bicv1,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_bicv1,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_bicv1,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_cbh1s,att_unt,unt_hgt);                 call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_cbh2s,att_unt,unt_hgt);                 call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_cbh3s,att_unt,unt_hgt);                 call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_cbh1s,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_cbh2s,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_cbh3s,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_blh1s,att_unt,unt_hgt);                 call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_blh2s,att_unt,unt_hgt);                 call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_blh3s,att_unt,unt_hgt);                 call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_blh1s,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_blh2s,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_blh3s,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_bli1s,att_unt,unt_hgt);                 call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_bli2s,att_unt,unt_hgt);                 call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_bli3s,att_unt,unt_hgt);                 call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_bli1s,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_bli2s,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_bli3s,'_FillValue',fv);                 call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_bscs,att_unt,unt_bsc);                  call chkerr(status,'unit')
status = nf90_put_att(ncid,vid_bscs,'_FillValue',fv);                  call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_bsct,'_FillValue',fv);                  call chkerr(status,'Fillvalue')
status = nf90_put_att(ncid,vid_bscv,'_FillValue',fv);                  call chkerr(status,'Fillvalue')
status = nf90_enddef(ncid)
status = nf90_put_var(ncid,vid_cbct,count_cbh);                        call chkerr(status,'cbct3s in')
status = nf90_put_var(ncid,vid_cbcv1,valid_cbh1);                      call chkerr(status,'cbcv11s in')
status = nf90_put_var(ncid,vid_cbcv2,valid_cbh2);                      call chkerr(status,'cbcv12s in')
status = nf90_put_var(ncid,vid_cbcv3,valid_cbh3);                      call chkerr(status,'cbct3s in')
status = nf90_put_var(ncid,vid_bhct,count_blh);                        call chkerr(status,'bhct3s in')
status = nf90_put_var(ncid,vid_bhcv1,valid_blh1);                      call chkerr(status,'bhcv11s in')
status = nf90_put_var(ncid,vid_bhcv2,valid_blh2);                      call chkerr(status,'bhcv12s in')
status = nf90_put_var(ncid,vid_bhcv3,valid_blh3);                      call chkerr(status,'bhct3s in')
status = nf90_put_var(ncid,vid_bicv1,valid_bli1);                      call chkerr(status,'bicv11s in')
status = nf90_put_var(ncid,vid_bicv2,valid_bli2);                      call chkerr(status,'bicv12s in')
status = nf90_put_var(ncid,vid_bicv3,valid_bli3);                      call chkerr(status,'bict3s in')
status = nf90_put_var(ncid,vid_cbh1s,cbh1);                            call chkerr(status,'cbh1s in')
status = nf90_put_var(ncid,vid_cbh2s,cbh2);                            call chkerr(status,'cbh2s in')
status = nf90_put_var(ncid,vid_cbh3s,cbh3);                            call chkerr(status,'cbh3s in')
status = nf90_put_var(ncid,vid_blh1s,blh1);                            call chkerr(status,'blh1s in')
status = nf90_put_var(ncid,vid_blh2s,blh2);                            call chkerr(status,'blh2s in')
status = nf90_put_var(ncid,vid_blh3s,blh3);                            call chkerr(status,'blh3s in')
status = nf90_put_var(ncid,vid_bli1s,bli1);                            call chkerr(status,'bli1s in')
status = nf90_put_var(ncid,vid_bli2s,bli2);                            call chkerr(status,'bli2s in')
status = nf90_put_var(ncid,vid_bli3s,bli3);                            call chkerr(status,'bli3s in')
status = nf90_put_var(ncid,vid_tims,ts);                               call chkerr(status,'t in')
status = nf90_put_var(ncid,vid_hgts,hs);                               call chkerr(status,'h in')
status = nf90_put_var(ncid,vid_bscs,bscs);                             call chkerr(status,'bsc in')
status = nf90_put_var(ncid,vid_bsct,count_bsc);                        call chkerr(status,'bsct in')
status = nf90_put_var(ncid,vid_bscv,valid_bsc);                        call chkerr(status,'bscv in')
status = nf90_close(ncid);                                             call chkerr(status,'end')

end subroutine ncwrite
subroutine chkerr(status,msg)
use netcdf
implicit none
integer,intent(in)::status
character(*)::msg
if(status.eq.nf90_noerr)then
        print*,'stage: ',msg,'  NetCDF F90 library message: ',NF90_STRERROR(status),'success'
else
        print*,'stage: ',msg,'  NetCDF F90 library message: ',NF90_STRERROR(status)
        stop 'stop'
endif
endsubroutine
