package org.apache.tools.ant.util.regexp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class RegexpUtilDiffblueTest {
  /**
  * Method under test: {@link RegexpUtil#asOptions(String)}
  */
  @Test
  public void testAsOptions() {
    // Arrange, Act and Assert
    assertEquals(65552, RegexpUtil.asOptions("Flags"));
    assertEquals(0, RegexpUtil.asOptions((String) null));
    assertEquals(RegexpMatcher.MATCH_CASE_INSENSITIVE, RegexpUtil.asOptions("i"));
    assertEquals(RegexpMatcher.MATCH_MULTILINE, RegexpUtil.asOptions("m"));
    assertEquals(0, RegexpUtil.asOptions(true));
    assertEquals(RegexpMatcher.MATCH_CASE_INSENSITIVE, RegexpUtil.asOptions(false));
    assertEquals(69632, RegexpUtil.asOptions(true, true, true));
    assertEquals(69888, RegexpUtil.asOptions(false, true, true));
    assertEquals(RegexpMatcher.MATCH_SINGLELINE, RegexpUtil.asOptions(true, false, true));
    assertEquals(RegexpMatcher.MATCH_MULTILINE, RegexpUtil.asOptions(true, true, false));
  }

  /**
   * Method under test: {@link RegexpUtil#hasFlag(int, int)}
   */
  @Test
  public void testHasFlag() {
    // Arrange, Act and Assert
    assertTrue(RegexpUtil.hasFlag(1, 1));
    assertFalse(RegexpUtil.hasFlag(0, 1));
  }

  /**
   * Method under test: {@link RegexpUtil#removeFlag(int, int)}
   */
  @Test
  public void testRemoveFlag() {
    // Arrange, Act and Assert
    assertEquals(0, RegexpUtil.removeFlag(1, 1));
    assertEquals(-2, RegexpUtil.removeFlag(-1, 1));
    assertEquals(2, RegexpUtil.removeFlag(3, 1));
    assertEquals(0, RegexpUtil.removeFlag(0, 1));
  }
}

