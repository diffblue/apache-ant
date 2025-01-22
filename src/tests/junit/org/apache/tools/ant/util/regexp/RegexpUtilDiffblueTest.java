package org.apache.tools.ant.util.regexp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class RegexpUtilDiffblueTest {
  /**
   * Test {@link RegexpUtil#hasFlag(int, int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpUtil#hasFlag(int, int)}
   */
  @Test
  public void testHasFlag_whenOne_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(RegexpUtil.hasFlag(1, 1));
  }

  /**
   * Test {@link RegexpUtil#hasFlag(int, int)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpUtil#hasFlag(int, int)}
   */
  @Test
  public void testHasFlag_whenZero_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(RegexpUtil.hasFlag(0, 1));
  }

  /**
   * Test {@link RegexpUtil#removeFlag(int, int)}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then return minus four.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpUtil#removeFlag(int, int)}
   */
  @Test
  public void testRemoveFlag_whenMinusOne_thenReturnMinusFour() {
    // Arrange, Act and Assert
    assertEquals(-4, RegexpUtil.removeFlag(-1, 3));
  }

  /**
   * Test {@link RegexpUtil#removeFlag(int, int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpUtil#removeFlag(int, int)}
   */
  @Test
  public void testRemoveFlag_whenOne_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, RegexpUtil.removeFlag(1, 3));
  }

  /**
   * Test {@link RegexpUtil#removeFlag(int, int)}.
   * <ul>
   *   <li>When three.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpUtil#removeFlag(int, int)}
   */
  @Test
  public void testRemoveFlag_whenThree_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, RegexpUtil.removeFlag(3, 3));
  }

  /**
   * Test {@link RegexpUtil#removeFlag(int, int)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpUtil#removeFlag(int, int)}
   */
  @Test
  public void testRemoveFlag_whenZero_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, RegexpUtil.removeFlag(0, 3));
  }

  /**
   * Test {@link RegexpUtil#asOptions(boolean, boolean, boolean)} with {@code caseSensitive}, {@code multiLine}, {@code singleLine}.
   * <p>
   * Method under test: {@link RegexpUtil#asOptions(boolean, boolean, boolean)}
   */
  @Test
  public void testAsOptionsWithCaseSensitiveMultiLineSingleLine() {
    // Arrange, Act and Assert
    assertEquals(RegexpMatcher.MATCH_CASE_INSENSITIVE, RegexpUtil.asOptions(false, false, false));
  }

  /**
   * Test {@link RegexpUtil#asOptions(boolean, boolean, boolean)} with {@code caseSensitive}, {@code multiLine}, {@code singleLine}.
   * <ul>
   *   <li>When {@code true}.</li>
   *   <li>Then return {@code 69632}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpUtil#asOptions(boolean, boolean, boolean)}
   */
  @Test
  public void testAsOptionsWithCaseSensitiveMultiLineSingleLine_whenTrue_thenReturn69632() {
    // Arrange, Act and Assert
    assertEquals(69632, RegexpUtil.asOptions(true, true, true));
  }

  /**
   * Test {@link RegexpUtil#asOptions(boolean)} with {@code caseSensitive}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@link RegexpMatcher#MATCH_CASE_INSENSITIVE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpUtil#asOptions(boolean)}
   */
  @Test
  public void testAsOptionsWithCaseSensitive_whenFalse_thenReturnMatch_case_insensitive() {
    // Arrange, Act and Assert
    assertEquals(RegexpMatcher.MATCH_CASE_INSENSITIVE, RegexpUtil.asOptions(false));
  }

  /**
   * Test {@link RegexpUtil#asOptions(boolean)} with {@code caseSensitive}.
   * <ul>
   *   <li>When {@code true}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpUtil#asOptions(boolean)}
   */
  @Test
  public void testAsOptionsWithCaseSensitive_whenTrue_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, RegexpUtil.asOptions(true));
  }

  /**
   * Test {@link RegexpUtil#asOptions(String)} with {@code flags}.
   * <ul>
   *   <li>When {@code Flags}.</li>
   *   <li>Then return {@code 65552}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpUtil#asOptions(String)}
   */
  @Test
  public void testAsOptionsWithFlags_whenFlags_thenReturn65552() {
    // Arrange, Act and Assert
    assertEquals(65552, RegexpUtil.asOptions("Flags"));
  }

  /**
   * Test {@link RegexpUtil#asOptions(String)} with {@code flags}.
   * <ul>
   *   <li>When {@code i}.</li>
   *   <li>Then return {@link RegexpMatcher#MATCH_CASE_INSENSITIVE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpUtil#asOptions(String)}
   */
  @Test
  public void testAsOptionsWithFlags_whenI_thenReturnMatch_case_insensitive() {
    // Arrange, Act and Assert
    assertEquals(RegexpMatcher.MATCH_CASE_INSENSITIVE, RegexpUtil.asOptions("i"));
  }

  /**
   * Test {@link RegexpUtil#asOptions(String)} with {@code flags}.
   * <ul>
   *   <li>When {@code m}.</li>
   *   <li>Then return {@link RegexpMatcher#MATCH_MULTILINE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpUtil#asOptions(String)}
   */
  @Test
  public void testAsOptionsWithFlags_whenM_thenReturnMatch_multiline() {
    // Arrange, Act and Assert
    assertEquals(RegexpMatcher.MATCH_MULTILINE, RegexpUtil.asOptions("m"));
  }

  /**
   * Test {@link RegexpUtil#asOptions(String)} with {@code flags}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpUtil#asOptions(String)}
   */
  @Test
  public void testAsOptionsWithFlags_whenNull_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, RegexpUtil.asOptions(null));
  }
}
