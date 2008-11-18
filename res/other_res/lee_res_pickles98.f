        program lee_res_pickles98
c
        open(10,file='tbl7_uifilt.txt',status='old',form='formatted')
        open(20,file='U3_pickles98.dat',status='unknown',
     +   form='formatted')
        write(20,'(a)') '       24     U3_pickles98.dat'
        do i=1,4
          read(10,*)
        end do
        do i=1,66
          read(10,*) ldo1,r1,ldo2,r2,ldo3,r3,ldo4,r4,ldo5,r5,ldo6,r6
          if(i.le.24) write(20,'(i4,5x,f5.3)') ldo1,r1
        end do
        close(10)
        close(20)
c
        open(10,file='tbl7_uifilt.txt',status='old',form='formatted')
        open(20,file='B2_pickles98.dat',status='unknown',
     +   form='formatted')
        write(20,'(a)') '       40     B2_pickles98.dat'
        do i=1,4
          read(10,*)
        end do
        do i=1,66
          read(10,*) ldo1,r1,ldo2,r2,ldo3,r3,ldo4,r4,ldo5,r5,ldo6,r6
          if(i.le.40) write(20,'(i4,5x,f5.3)') ldo2,r2
        end do
        close(10)
        close(20)
c
        open(10,file='tbl7_uifilt.txt',status='old',form='formatted')
        open(20,file='B3_pickles98.dat',status='unknown',
     +   form='formatted')
        write(20,'(a)') '       40     B3_pickles98.dat'
        do i=1,4
          read(10,*)
        end do
        do i=1,66
          read(10,*) ldo1,r1,ldo2,r2,ldo3,r3,ldo4,r4,ldo5,r5,ldo6,r6
          if(i.le.40) write(20,'(i4,5x,f5.3)') ldo3,r3
        end do
        close(10)
        close(20)
c
        open(10,file='tbl7_uifilt.txt',status='old',form='formatted')
        open(20,file='V_pickles98.dat',status='unknown',
     +   form='formatted')
        write(20,'(a)') '       54     V_pickles98.dat'
        do i=1,4
          read(10,*)
        end do
        do i=1,66
          read(10,*) ldo1,r1,ldo2,r2,ldo3,r3,ldo4,r4,ldo5,r5,ldo6,r6
          if(i.le.54) write(20,'(i4,5x,f5.3)') ldo4,r4
        end do
        close(10)
        close(20)
c
        open(10,file='tbl7_uifilt.txt',status='old',form='formatted')
        open(20,file='Rc_pickles98.dat',status='unknown',
     +   form='formatted')
        write(20,'(a)') '       66     Rc_pickles98.dat'
        do i=1,4
          read(10,*)
        end do
        do i=1,66
          read(10,*) ldo1,r1,ldo2,r2,ldo3,r3,ldo4,r4,ldo5,r5,ldo6,r6
          if(i.le.66) write(20,'(i4,5x,f5.3)') ldo5,r5
        end do
        close(10)
        close(20)
c
        open(10,file='tbl7_uifilt.txt',status='old',form='formatted')
        open(20,file='Ic_pickles98.dat',status='unknown',
     +   form='formatted')
        write(20,'(a)') '       39     Ic_pickles98.dat'
        do i=1,4
          read(10,*)
        end do
        do i=1,66
          read(10,*) ldo1,r1,ldo2,r2,ldo3,r3,ldo4,r4,ldo5,r5,ldo6,r6
          if(i.le.39) write(20,'(i4,5x,f5.3)') ldo6,r6
        end do
        close(10)
        close(20)
c
        open(10,file='tbl8_jmfilt.txt',status='old',form='formatted')
        open(20,file='J_pickles98.dat',status='unknown',
     +   form='formatted')
        write(20,'(a)') '       21     J_pickles98.dat'
        do i=1,3
          read(10,*)
        end do
        do i=1,25
          read(10,*) ldo1,r1,ldo2,r2,ldo3,r3,ldo4,r4,ldo5,r5,ldo6,r6
          if(i.le.21) write(20,'(i5,4x,f5.3)') ldo1,r1
        end do
        close(10)
        close(20)
c
        open(10,file='tbl8_jmfilt.txt',status='old',form='formatted')
        open(20,file='H_pickles98.dat',status='unknown',
     +   form='formatted')
        write(20,'(a)') '       21     H_pickles98.dat'
        do i=1,3
          read(10,*)
        end do
        do i=1,25
          read(10,*) ldo1,r1,ldo2,r2,ldo3,r3,ldo4,r4,ldo5,r5,ldo6,r6
          if(i.le.21) write(20,'(i5,4x,f5.3)') ldo2,r2
        end do
        close(10)
        close(20)
c
        open(10,file='tbl8_jmfilt.txt',status='old',form='formatted')
        open(20,file='K_pickles98.dat',status='unknown',
     +   form='formatted')
        write(20,'(a)') '       25     K_pickles98.dat'
        do i=1,3
          read(10,*)
        end do
        do i=1,25
          read(10,*) ldo1,r1,ldo2,r2,ldo3,r3,ldo4,r4,ldo5,r5,ldo6,r6
          if(i.le.25) write(20,'(i5,4x,f5.3)') ldo3,r3
        end do
        close(10)
        close(20)
c
        open(10,file='tbl8_jmfilt.txt',status='old',form='formatted')
        open(20,file='L_pickles98.dat',status='unknown',
     +   form='formatted')
        write(20,'(a)') '       21     L_pickles98.dat'
        do i=1,3
          read(10,*)
        end do
        do i=1,25
          read(10,*) ldo1,r1,ldo2,r2,ldo3,r3,ldo4,r4,ldo5,r5,ldo6,r6
          if(i.le.21) write(20,'(i5,4x,f5.3)') ldo4,r4
        end do
        close(10)
        close(20)
c
        open(10,file='tbl8_jmfilt.txt',status='old',form='formatted')
        open(20,file='Lp_pickles98.dat',status='unknown',
     +   form='formatted')
        write(20,'(a)') '       20     Lp_pickles98.dat'
        do i=1,3
          read(10,*)
        end do
        do i=1,25
          read(10,*) ldo1,r1,ldo2,r2,ldo3,r3,ldo4,r4,ldo5,r5,ldo6,r6
          if(i.le.20) write(20,'(i5,4x,f5.3)') ldo5,r5
        end do
        close(10)
        close(20)
c
        open(10,file='tbl8_jmfilt.txt',status='old',form='formatted')
        open(20,file='M_pickles98.dat',status='unknown',
     +   form='formatted')
        write(20,'(a)') '       19     M_pickles98.dat'
        do i=1,3
          read(10,*)
        end do
        do i=1,25
          read(10,*) ldo1,r1,ldo2,r2,ldo3,r3,ldo4,r4,ldo5,r5,ldo6,r6
          if(i.le.19) write(20,'(i5,4x,f5.3)') ldo6,r6
        end do
        close(10)
        close(20)
c
        stop
        end
