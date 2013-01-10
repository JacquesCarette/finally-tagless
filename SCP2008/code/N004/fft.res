Meta Caml toplevel, version N 004
        Objective Caml version 3.11.2

# #                     * *                                                                                   val impulse : int -> int -> (float * float) list = <fun>
val unpair : ('a * 'a) list -> 'a list = <fun>
val inf_norm : float list -> float list -> int * float = <fun>
val ttt3 : ('a, float array -> float array) code =
  .<fun x_1 ->
     let t_2 = x_1.(0) in
     let t_3 = x_1.(1) in
     let t_4 = x_1.(2) in
     let t_5 = x_1.(3) in
     let t_6 = x_1.(4) in
     let t_7 = x_1.(5) in
     let t_8 = x_1.(6) in
     let t_9 = x_1.(7) in
     let t_10 = (t_2 +. t_6) in
     let t_11 = (t_3 +. t_7) in
     let t_12 = (t_2 -. t_6) in
     let t_13 = (t_3 -. t_7) in
     let t_14 = (t_4 +. t_8) in
     let t_15 = (t_5 +. t_9) in
     let t_16 = (t_4 -. t_8) in
     let t_17 = (t_5 -. t_9) in
     let t_18 = (t_10 +. t_14) in
     let t_19 = (t_11 +. t_15) in
     let t_20 = (t_10 -. t_14) in
     let t_21 = (t_11 -. t_15) in
     let t_22 = (t_12 -. t_17) in
     let t_23 = (t_13 +. t_16) in
     let t_24 = (t_12 +. t_17) in
     let t_25 = (t_13 -. t_16) in
     x_1.(0) <- t_18;
     x_1.(1) <- t_19;
     x_1.(2) <- t_24;
     x_1.(3) <- t_25;
     x_1.(4) <- t_20;
     x_1.(5) <- t_21;
     x_1.(6) <- t_22;
     x_1.(7) <- t_23;
     x_1>.
val test_ad : Fft.dir -> (float * float) list -> (float * float) list = <fun>
val ttt4 : ('a, float array -> float array) code =
  .<fun x_592 ->
     let t_593 = x_592.(0) in
     let t_594 = x_592.(1) in
     let t_595 = x_592.(2) in
     let t_596 = x_592.(3) in
     let t_597 = x_592.(4) in
     let t_598 = x_592.(5) in
     let t_599 = x_592.(6) in
     let t_600 = x_592.(7) in
     let t_601 = x_592.(8) in
     let t_602 = x_592.(9) in
     let t_603 = x_592.(10) in
     let t_604 = x_592.(11) in
     let t_605 = x_592.(12) in
     let t_606 = x_592.(13) in
     let t_607 = x_592.(14) in
     let t_608 = x_592.(15) in
     let t_609 = x_592.(16) in
     let t_610 = x_592.(17) in
     let t_611 = x_592.(18) in
     let t_612 = x_592.(19) in
     let t_613 = x_592.(20) in
     let t_614 = x_592.(21) in
     let t_615 = x_592.(22) in
     let t_616 = x_592.(23) in
     let t_617 = x_592.(24) in
     let t_618 = x_592.(25) in
     let t_619 = x_592.(26) in
     let t_620 = x_592.(27) in
     let t_621 = x_592.(28) in
     let t_622 = x_592.(29) in
     let t_623 = x_592.(30) in
     let t_624 = x_592.(31) in
     let t_625 = (t_593 +. t_609) in
     let t_626 = (t_594 +. t_610) in
     let t_627 = (t_593 -. t_609) in
     let t_628 = (t_594 -. t_610) in
     let t_629 = (t_595 +. t_611) in
     let t_630 = (t_596 +. t_612) in
     let t_631 = (t_595 -. t_611) in
     let t_632 = (t_596 -. t_612) in
     let t_633 = (t_597 +. t_613) in
     let t_634 = (t_598 +. t_614) in
     let t_635 = (t_597 -. t_613) in
     let t_636 = (t_598 -. t_614) in
     let t_637 = (t_599 +. t_615) in
     let t_638 = (t_600 +. t_616) in
     let t_639 = (t_599 -. t_615) in
     let t_640 = (t_600 -. t_616) in
     let t_641 = (t_601 +. t_617) in
     let t_642 = (t_602 +. t_618) in
     let t_643 = (t_601 -. t_617) in
     let t_644 = (t_602 -. t_618) in
     let t_645 = (t_603 +. t_619) in
     let t_646 = (t_604 +. t_620) in
     let t_647 = (t_603 -. t_619) in
     let t_648 = (t_604 -. t_620) in
     let t_649 = (t_605 +. t_621) in
     let t_650 = (t_606 +. t_622) in
     let t_651 = (t_605 -. t_621) in
     let t_652 = (t_606 -. t_622) in
     let t_653 = (t_607 +. t_623) in
     let t_654 = (t_608 +. t_624) in
     let t_655 = (t_607 -. t_623) in
     let t_656 = (t_608 -. t_624) in
     let t_657 = (t_625 +. t_641) in
     let t_658 = (t_626 +. t_642) in
     let t_659 = (t_625 -. t_641) in
     let t_660 = (t_626 -. t_642) in
     let t_661 = (t_629 +. t_645) in
     let t_662 = (t_630 +. t_646) in
     let t_663 = (t_629 -. t_645) in
     let t_664 = (t_630 -. t_646) in
     let t_665 = (t_633 +. t_649) in
     let t_666 = (t_634 +. t_650) in
     let t_667 = (t_633 -. t_649) in
     let t_668 = (t_634 -. t_650) in
     let t_669 = (t_637 +. t_653) in
     let t_670 = (t_638 +. t_654) in
     let t_671 = (t_637 -. t_653) in
     let t_672 = (t_638 -. t_654) in
     let t_673 = (t_657 +. t_665) in
     let t_674 = (t_658 +. t_666) in
     let t_675 = (t_657 -. t_665) in
     let t_676 = (t_658 -. t_666) in
     let t_677 = (t_661 +. t_669) in
     let t_678 = (t_662 +. t_670) in
     let t_679 = (t_661 -. t_669) in
     let t_680 = (t_662 -. t_670) in
     let t_681 = (t_673 +. t_677) in
     let t_682 = (t_674 +. t_678) in
     let t_683 = (t_673 -. t_677) in
     let t_684 = (t_674 -. t_678) in
     let t_685 = (t_675 -. t_680) in
     let t_686 = (t_676 +. t_679) in
     let t_687 = (t_675 +. t_680) in
     let t_688 = (t_676 -. t_679) in
     let t_689 = (t_659 -. t_668) in
     let t_690 = (t_660 +. t_667) in
     let t_691 = (t_659 +. t_668) in
     let t_692 = (t_660 -. t_667) in
     let t_693 = (t_663 -. t_672) in
     let t_694 = (t_664 +. t_671) in
     let t_695 = (t_663 +. t_672) in
     let t_696 = (t_664 -. t_671) in
     let t_697 = (0.707106781187 *. (t_695 -. t_696)) in
     let t_698 = (0.707106781187 *. (t_695 +. t_696)) in
     let t_699 = (t_691 -. t_698) in
     let t_700 = (t_692 +. t_697) in
     let t_701 = (t_691 +. t_698) in
     let t_702 = (t_692 -. t_697) in
     let t_703 = (0.707106781187 *. (t_693 -. t_694)) in
     let t_704 = (0.707106781187 *. (t_693 +. t_694)) in
     let t_705 = (t_689 +. t_703) in
     let t_706 = (t_690 +. t_704) in
     let t_707 = (t_689 -. t_703) in
     let t_708 = (t_690 -. t_704) in
     let t_709 = (t_627 -. t_644) in
     let t_710 = (t_628 +. t_643) in
     let t_711 = (t_627 +. t_644) in
     let t_712 = (t_628 -. t_643) in
     let t_713 = (t_631 -. t_648) in
     let t_714 = (t_632 +. t_647) in
     let t_715 = (t_631 +. t_648) in
     let t_716 = (t_632 -. t_647) in
     let t_717 = (t_635 -. t_652) in
     let t_718 = (t_636 +. t_651) in
     let t_719 = (t_635 +. t_652) in
     let t_720 = (t_636 -. t_651) in
     let t_721 = (t_639 -. t_656) in
     let t_722 = (t_640 +. t_655) in
     let t_723 = (t_639 +. t_656) in
     let t_724 = (t_640 -. t_655) in
     let t_725 = (0.707106781187 *. (t_719 -. t_720)) in
     let t_726 = (0.707106781187 *. (t_719 +. t_720)) in
     let t_727 = (t_711 -. t_726) in
     let t_728 = (t_712 +. t_725) in
     let t_729 = (t_711 +. t_726) in
     let t_730 = (t_712 -. t_725) in
     let t_731 = (0.382683432365 *. (t_715 +. t_716)) in
     let t_732 = (1.30656296488 *. t_716) in
     let t_733 = (0.541196100146 *. t_715) in
     let t_734 = (t_731 -. t_732) in
     let t_735 = (t_731 +. t_733) in
     let t_736 = (0.923879532511 *. (t_723 +. t_724)) in
     let t_737 = (1.30656296488 *. t_724) in
     let t_738 = (0.541196100146 *. t_723) in
     let t_739 = (t_736 -. t_737) in
     let t_740 = (t_736 -. t_738) in
     let t_741 = (t_734 +. t_739) in
     let t_742 = (t_735 +. t_740) in
     let t_743 = (t_734 -. t_739) in
     let t_744 = (t_735 -. t_740) in
     let t_745 = (t_729 -. t_742) in
     let t_746 = (t_730 +. t_741) in
     let t_747 = (t_729 +. t_742) in
     let t_748 = (t_730 -. t_741) in
     let t_749 = (t_727 +. t_743) in
     let t_750 = (t_728 +. t_744) in
     let t_751 = (t_727 -. t_743) in
     let t_752 = (t_728 -. t_744) in
     let t_753 = (0.707106781187 *. (t_717 -. t_718)) in
     let t_754 = (0.707106781187 *. (t_717 +. t_718)) in
     let t_755 = (t_709 +. t_753) in
     let t_756 = (t_710 +. t_754) in
     let t_757 = (t_709 -. t_753) in
     let t_758 = (t_710 -. t_754) in
     let t_759 = (0.923879532511 *. (t_713 +. t_714)) in
     let t_760 = (1.30656296488 *. t_714) in
     let t_761 = (0.541196100146 *. t_713) in
     let t_762 = (t_759 -. t_760) in
     let t_763 = (t_759 -. t_761) in
     let t_764 = (0.382683432365 *. (t_721 +. t_722)) in
     let t_765 = (1.30656296488 *. t_722) in
     let t_766 = (0.541196100146 *. t_721) in
     let t_767 = (t_764 -. t_765) in
     let t_768 = (t_764 +. t_766) in
     let t_769 = (t_762 +. t_767) in
     let t_770 = (t_763 +. t_768) in
     let t_771 = (t_762 -. t_767) in
     let t_772 = (t_763 -. t_768) in
     let t_773 = (t_757 -. t_772) in
     let t_774 = (t_758 +. t_771) in
     let t_775 = (t_757 +. t_772) in
     let t_776 = (t_758 -. t_771) in
     let t_777 = (t_755 +. t_769) in
     let t_778 = (t_756 +. t_770) in
     let t_779 = (t_755 -. t_769) in
     let t_780 = (t_756 -. t_770) in
     x_592.(0) <- t_681;
     x_592.(1) <- t_682;
     x_592.(2) <- t_747;
     x_592.(3) <- t_748;
     x_592.(4) <- t_701;
     x_592.(5) <- t_702;
     x_592.(6) <- t_775;
     x_592.(7) <- t_776;
     x_592.(8) <- t_687;
     x_592.(9) <- t_688;
     x_592.(10) <- t_751;
     x_592.(11) <- t_752;
     x_592.(12) <- t_707;
     x_592.(13) <- t_708;
     x_592.(14) <- t_779;
     x_592.(15) <- t_780;
     x_592.(16) <- t_683;
     x_592.(17) <- t_684;
     x_592.(18) <- t_745;
     x_592.(19) <- t_746;
     x_592.(20) <- t_699;
     x_592.(21) <- t_700;
     x_592.(22) <- t_773;
     x_592.(23) <- t_774;
     x_592.(24) <- t_685;
     x_592.(25) <- t_686;
     x_592.(26) <- t_749;
     x_592.(27) <- t_750;
     x_592.(28) <- t_705;
     x_592.(29) <- t_706;
     x_592.(30) <- t_777;
     x_592.(31) <- t_778;
     x_592>.
# 
