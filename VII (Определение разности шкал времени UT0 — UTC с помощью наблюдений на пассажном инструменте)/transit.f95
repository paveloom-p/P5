program in_out
implicit none
    
    real, dimension(29,11) :: observations_data         ! № КСВ, time in sec, level, alpha in sec, I, A, i, a
    real, dimension(24,7) :: catalog_data               ! № КСВ, hour, min, sec, alpha in sec, I, A
    
    integer i, j
    real A_zen, Az, a_eq, a, alpha_T, alpha_Tz, u, u_mean, r, t, y, r1, t1, y1
    
    open ( unit=69, file='observations.dat' )
    open ( unit=96, file='catalog.dat' )
    
    observations_data=0
    
    do i=1, 8                                                                                           ! это у нас так звёзды распределились до 24-альфа
    
        read(96,*) catalog_data(i,1:4), catalog_data(i,6:7)
        catalog_data(i,5) = catalog_data(i,2)*3600+catalog_data(i,3)*60+catalog_data(i,4)
!        write(*,*) catalog_data(i,5)
    
    enddo
    
    do i=9, 22
    
        read(96,*) catalog_data(i,1:4), catalog_data(i,6:7)
        catalog_data(i,5) = 86400 - ( catalog_data(i,2)*3600+catalog_data(i,3)*60+catalog_data(i,4) )
    
    enddo
    
    do i=23, 24                                                                                         ! и после 24-фльфа
    
        read(96,*) catalog_data(i,1:4), catalog_data(i,6:7)
        catalog_data(i,5) = catalog_data(i,2)*3600+catalog_data(i,3)*60+catalog_data(i,4)
    
    enddo
    
    do i=1, 29
    
        read(69,*) observations_data(i,1:6)
    
    enddo
    
    do i=1, 29
        do j=1, 24
        
            if(observations_data(i,1)==catalog_data(j,1)) then
                
                observations_data(i,7:9) = catalog_data(j,5:7)
            
            endif
            
        enddo
    enddo
    
    do i=1, 29                                                                                          ! считаем i, надо подумать о том где у вас W-O
        
        if(mod(i,2)==1) then
            
            observations_data(i,10)=(observations_data(i,5)+observations_data(i,6)-observations_data(i,3)-observations_data(i,4))&
            *0.042
        
        else
        
            observations_data(i,10)=(observations_data(i,3)+observations_data(i,4)-observations_data(i,5)-observations_data(i,6))&
            *0.042
            
        endif
        
    enddo
    
    
    j=0
    A_zen=0
    alpha_T=0
    
    observations_data(:,2)=observations_data(:,2)+observations_data(:,8)*observations_data(:,10)        ! высчитываем поправку ко времени за наклонность
    
    do i=1, 29                                                                                          ! вычисялем хар-ки зенитных звёзд
    
        if( observations_data(i,9) < 0.3 .AND. observations_data(i,9) > -0.5 .AND. observations_data(i,9) /=0 &
        .AND. observations_data(i,7) > 1) then
            
            j=j+1
            A_zen=A_zen+observations_data(i,9)
            alpha_T=alpha_T+( observations_data(i,7)-observations_data(i,2) )
            
        endif
    
    enddo
    
    Az=A_zen/j
    alpha_Tz=alpha_T/j
    
    write(*,'(a3,F7.4)') 'Az=',Az
    write(*,'(a10,F7.4)') 'alpha_Tz=', alpha_Tz
    
    j=0
    a_eq=0
        
    do i=1, 29                                                                                           ! вычисялем средний азимут
    
        if( observations_data(i,9) > 0.58 .AND. observations_data(i,7) > 1) then
            
            j=j+1
            observations_data(i,11)=( alpha_Tz - (observations_data(i,7)-observations_data(i,2)) )/(Az - observations_data(i,9) )
            a_eq=a_eq+( alpha_Tz - (observations_data(i,7)-observations_data(i,2)) )/(Az - observations_data(i,9) )
!            write(*,*) observations_data(i,1), ( alpha_Tz - (observations_data(i,7)-observations_data(i,2))&
!            )/(Az - observations_data(i,9) )
            
        endif
    
    enddo
    
    a=a_eq/j
    
    write(*,'(a3,F7.4)') 'a=', a
        
    u=0
    j=0
    
    do i=1, 29                                                                                              ! считаем поправку часов
    
        if(observations_data(i,7) > 1) then
            
            j=j+1
            u=u+observations_data(i,7)-observations_data(i,2)-observations_data(i,9)*a
!            write(*,*) observations_data(i,1),observations_data(i,7)-observations_data(i,2)-observations_data(i,9)*a
        
        endif
        
    enddo
    
    u_mean=u/j
    
    do i=9, 22                                                                                              ! делаем удобно для красивого вывода
        
        catalog_data(i,2) = (catalog_data(i,5)-mod(catalog_data(i,5),3600.0))/3600
        catalog_data(i,3) = (catalog_data(i,5) - r*3600)/60 - mod((catalog_data(i,5) - r*3600)/60,1.0)
        catalog_data(i,4) = mod((catalog_data(i,5) - r*3600)/60,1.0) * 60
    
        catalog_data(i,5) = 86400 - ( catalog_data(i,2)*3600+catalog_data(i,3)*60+catalog_data(i,4) )
    
    enddo
    
    
    write(*,'(a10,F7.4)') 'UT0-UTCm', u_mean
    
    do i=1,29                                                                                               ! также
        
        if(observations_data(i,7) > 1) then
    
            r = (observations_data(i,2)-mod(observations_data(i,2),3600.0))/3600
            t = (observations_data(i,2) - r*3600)/60 - mod((observations_data(i,2) - r*3600)/60,1.0)
            y = mod((observations_data(i,2) - r*3600)/60,1.0) * 60
    
            r1 = (observations_data(i,7)-mod(observations_data(i,7),3600.0))/3600
            t1 = (observations_data(i,7) - r*3600)/60 - mod((observations_data(i,7) - r*3600)/60,1.0)
            y1 = mod((observations_data(i,7) - r*3600)/60,1.0) * 60
    
            write(*,10) observations_data(i,1),'&',&                                                        ! тут всё как в табличке выводится
                observations_data(i,8),'&', observations_data(i,9),'&',&
                observations_data(i,10),'&', r1, t1, y1, '&', r, t, y ,'&',&
                observations_data(i,8)*observations_data(i,10),'&', observations_data(i,7)-observations_data(i,2),'&',&
                observations_data(i,11),'&', a*observations_data(i,9),'&',&
                observations_data(i,7)-observations_data(i,2)-observations_data(i,9)*a,'\\ \hline'
                
!           write(*,*) observations_data(i,1), r, t, y

            10 format (F4.0,a2,F7.3,a2,F7.3,a2,F7.3,a2,F4.0,F4.0,F7.4,a2,F5.0,F4.0,F7.4,a2,F7.4,a2,F7.4,a2,F7.4,a2,F7.4&
                ,a2,F7.4,a10)
        endif
    
    enddo
    
    close(69)
    close(96)
    
end program in_out
