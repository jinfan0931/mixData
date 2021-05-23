!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   mix different 2-dimension data with emissions to improve resolution       !
!   subroutine by: Jin Fan, Xiaoying Xu                                       !
!   sponsor: Chengdu Univ of Info Tech; Chengdu LanLan Enviro Tech Co. Ltd.   !
!   Q&A: jin.fan@outlook.com                                                  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine mix_data(ori_data,ref_data,ori_lon,ori_lat,ref_lon,ref_lat,ori_m,ori_n,ref_m,ref_n,ori_xRes,ori_yRes,xOri,yOri,xRef,&
    &yRef,mask,c,reg_tot,ratio,new,output)
    integer,intent(in) :: ori_m,ori_n,ref_m,ref_n
    real,intent(in) :: ori_data(ori_m,ori_n),ref_data(ref_m,ref_n),ori_lon(ori_m,ori_n),ref_lon(ref_m,ref_n),&
    ori_lat(ori_m,ori_n),ref_lat(ref_m,ref_n),ori_xRes,ori_yRes
    integer,intent(out) :: xOri(ori_m,ori_n),yOri(ori_m,ori_n),xRef(ref_m,ref_n),yRef(ref_m,ref_n)
    logical,intent(out) :: mask(ref_m,ref_n)
    real,intent(out) :: reg_tot(ref_m,ref_n),ratio(ref_m,ref_n),output(ref_m,ref_n),new(ref_m,ref_n)

    real,intent(out)::c
    integer i,j

    xOri=int((ori_lon-minval(ori_lon))/ori_xRes)
    yOri=int((ori_lat-minval(ori_lat))/ori_yRes)
    xRef=int((ref_lon-minval(ori_lon))/ori_xRes)
    yRef=int((ref_lat-minval(ori_lat))/ori_yRes)
    
    do i=minval(xRef),maxval(xRef)
        do j=minval(yRef),maxval(yRef)
            mask=(xRef==i .and. yRef==j)
            c=sum(ref_data,mask)
            if (c>0) then
                where (mask .eqv. .true.)
                    reg_tot=c
                    ratio=ref_data/reg_tot
                end where
            else
                where (mask .eqv. .true.)
                    ratio=0.0
                end where 
            end if
        end do
    end do

    do i=minval(xOri),maxval(xOri)
        do j=minval(yOri),maxval(yOri)
            mask=(xRef==i .and. yRef==j)
            where (mask .eqv. .true.)
                new=ori_data(j+1,i+1)
            end where
        end do
    end do

    output=new*ratio

end subroutine mix_data