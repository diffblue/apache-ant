package org.apache.tools.tar;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import org.apache.tools.zip.ZipEncoding;
import org.junit.Test;

public class TarUtilsDiffblueTest {
  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code 0}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_when0_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', '0', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code 0}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_when0_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', '0', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code 0}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_when0_thenThrowIllegalArgumentException3() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, '0', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code 7}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_when7_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', '7', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code 7}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_when7_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, '7', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException3() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 2, TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException4() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 3, TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException5() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 5, TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException6() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 1, TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException7() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', -1, TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException8() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils
        .parseOctal(new byte[]{'A', 'X', Byte.MAX_VALUE, TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException9() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, 2, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException10() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, 3, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException11() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, 5, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException12() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, 'X', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException13() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils
        .parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException14() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, 1, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException15() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, -1, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException16() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils
        .parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, Byte.MAX_VALUE, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException17() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 2));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException18() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', 'A', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException19() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', 2, TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException20() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', 3, TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenA_thenThrowIllegalArgumentException21() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', 5, TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code AX0XAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenAx0xaxaxBytesIsUtf8_thenThrowIllegalArgumentException()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils.parseOctal("AX0XAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code AX XAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenAxXaxaxBytesIsUtf8_thenThrowIllegalArgumentException()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils.parseOctal("AX XAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code AXAX XAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenAxaxXaxBytesIsUtf8_thenThrowIllegalArgumentException()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils.parseOctal("AXAX XAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenAxaxaxaxBytesIsUtf8_thenThrowIllegalArgumentException()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils.parseOctal("AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When backspace.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenBackspace_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', '\b', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When backspace.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenBackspace_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, '\b', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When backspace.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenBackspace_thenThrowIllegalArgumentException3() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', '\b', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When five.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenFive_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 5));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenOne_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'}, 2, 1));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenOne_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X', 'A', 'X'}, 1, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenOne_thenThrowIllegalArgumentException3() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 1, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When space.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenSpace_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', ' ', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When space.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenSpace_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', ' ', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When space.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenSpace_thenThrowIllegalArgumentException3() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, ' ', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When space.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenSpace_thenThrowIllegalArgumentException4() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', ' ', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When tab.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenTab_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', '\t', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When tab.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenTab_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, '\t', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When tab.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenTab_thenThrowIllegalArgumentException3() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', '\t', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenX_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0L, TarUtils.parseOctal(new byte[]{'A', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenX_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 2, 'X', 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenX_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 2, 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenX_thenThrowIllegalArgumentException3() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'X', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenX_thenThrowIllegalArgumentException4() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 3, 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenX_thenThrowIllegalArgumentException5() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 5, 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenX_thenThrowIllegalArgumentException6() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 'X', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenX_thenThrowIllegalArgumentException7() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 1, 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenX_thenThrowIllegalArgumentException8() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', -1, 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenX_thenThrowIllegalArgumentException9() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctal(new byte[]{'A', 'X', 4, 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctal(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctal(byte[], int, int)}
   */
  @Test
  public void testParseOctal_whenX_thenThrowIllegalArgumentException10() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils
        .parseOctal(new byte[]{'A', 'X', Byte.MAX_VALUE, 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code 0}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_when0_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils
        .parseOctalOrBinary(new byte[]{'A', 'X', '0', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code 0}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_when0_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils
        .parseOctalOrBinary(new byte[]{'A', 'X', '0', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenA_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils
        .parseOctalOrBinary(new byte[]{'A', 'X', 'A', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenA_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils
        .parseOctalOrBinary(new byte[]{'A', 'X', 'A', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenA_thenThrowIllegalArgumentException3() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctalOrBinary(new byte[]{'A', 'X', 2, TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenA_thenThrowIllegalArgumentException4() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctalOrBinary(new byte[]{'A', 'X', 3, TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenA_thenThrowIllegalArgumentException5() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctalOrBinary(new byte[]{'A', 'X', 5, TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code AX0XAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenAx0xaxaxBytesIsUtf8_thenThrowIllegalArgumentException()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils.parseOctalOrBinary("AX0XAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code AX XAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenAxXaxaxBytesIsUtf8_thenThrowIllegalArgumentException()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils.parseOctalOrBinary("AX XAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code AXAX XAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenAxaxXaxBytesIsUtf8_thenThrowIllegalArgumentException()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils.parseOctalOrBinary("AXAX XAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenAxaxaxaxBytesIsUtf8_thenThrowIllegalArgumentException()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils.parseOctalOrBinary("AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When backspace.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenBackspace_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils
        .parseOctalOrBinary(new byte[]{'A', 'X', '\b', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenOne_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctalOrBinary(new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'}, 2, 1));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenOne_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils
        .parseOctalOrBinary(new byte[]{'A', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X', 'A', 'X'}, 1, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When space.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenSpace_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils
        .parseOctalOrBinary(new byte[]{'A', 'X', ' ', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When space.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenSpace_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils
        .parseOctalOrBinary(new byte[]{'A', 'X', ' ', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then return {@code 22593}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenX_thenReturn22593() {
    // Arrange, Act and Assert
    assertEquals(22593L,
        TarUtils.parseOctalOrBinary(new byte[]{'A', 'X', Byte.MIN_VALUE, 'X', 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then return {@code -42943}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenX_thenReturn42943() {
    // Arrange, Act and Assert
    assertEquals(-42943L, TarUtils.parseOctalOrBinary(new byte[]{'A', 'X', -1, 'X', 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenX_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0L,
        TarUtils.parseOctalOrBinary(new byte[]{'A', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenX_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctalOrBinary(new byte[]{'A', 'X', 2, 'X', 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenX_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.parseOctalOrBinary(new byte[]{'A', 'X', 2, 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseOctalOrBinary(byte[], int, int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseOctalOrBinary(byte[], int, int)}
   */
  @Test
  public void testParseOctalOrBinary_whenX_thenThrowIllegalArgumentException3() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> TarUtils
        .parseOctalOrBinary(new byte[]{'A', 'X', 'X', TarConstants.LF_OLDNORM, 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseBoolean(byte[], int)}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseBoolean(byte[], int)}
   */
  @Test
  public void testParseBoolean_whenAxaxaxaxBytesIsUtf8_thenReturnFalse() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertFalse(TarUtils.parseBoolean("AXAXAXAX".getBytes("UTF-8"), 2));
  }

  /**
   * Test {@link TarUtils#parseBoolean(byte[], int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseBoolean(byte[], int)}
   */
  @Test
  public void testParseBoolean_whenX_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(TarUtils.parseBoolean(new byte[]{'A', 'X', 1, 'X', 'A', 'X', 'A', 'X'}, 2));
  }

  /**
   * Test {@link TarUtils#parseName(byte[], int, int, ZipEncoding)} with {@code buffer}, {@code offset}, {@code length}, {@code encoding}.
   * <ul>
   *   <li>Then return {@code AXA}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseName(byte[], int, int, ZipEncoding)}
   */
  @Test
  public void testParseNameWithBufferOffsetLengthEncoding_thenReturnAxa() throws IOException {
    // Arrange, Act and Assert
    assertEquals("AXA", TarUtils.parseName("AXAXAXAX".getBytes("UTF-8"), 2, 3, TarUtils.DEFAULT_ENCODING));
  }

  /**
   * Test {@link TarUtils#parseName(byte[], int, int, ZipEncoding)} with {@code buffer}, {@code offset}, {@code length}, {@code encoding}.
   * <ul>
   *   <li>When {@link TarUtils#FALLBACK_ENCODING}.</li>
   *   <li>Then return {@code AXA}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseName(byte[], int, int, ZipEncoding)}
   */
  @Test
  public void testParseNameWithBufferOffsetLengthEncoding_whenFallback_encoding_thenReturnAxa() throws IOException {
    // Arrange, Act and Assert
    assertEquals("AXA", TarUtils.parseName("AXAXAXAX".getBytes("UTF-8"), 2, 3, TarUtils.FALLBACK_ENCODING));
  }

  /**
   * Test {@link TarUtils#parseName(byte[], int, int, ZipEncoding)} with {@code buffer}, {@code offset}, {@code length}, {@code encoding}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseName(byte[], int, int, ZipEncoding)}
   */
  @Test
  public void testParseNameWithBufferOffsetLengthEncoding_whenX_thenReturnEmptyString() throws IOException {
    // Arrange, Act and Assert
    assertEquals("", TarUtils.parseName(new byte[]{'A', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X', 'A', 'X'}, 2, 3,
        TarUtils.DEFAULT_ENCODING));
  }

  /**
   * Test {@link TarUtils#parseName(byte[], int, int, ZipEncoding)} with {@code buffer}, {@code offset}, {@code length}, {@code encoding}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseName(byte[], int, int, ZipEncoding)}
   */
  @Test
  public void testParseNameWithBufferOffsetLengthEncoding_whenZero_thenReturnEmptyString() throws IOException {
    // Arrange, Act and Assert
    assertEquals("",
        TarUtils.parseName(new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'}, 2, 0, TarUtils.DEFAULT_ENCODING));
  }

  /**
   * Test {@link TarUtils#parseName(byte[], int, int)} with {@code buffer}, {@code offset}, {@code length}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return {@code AXA}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseName(byte[], int, int)}
   */
  @Test
  public void testParseNameWithBufferOffsetLength_whenAxaxaxaxBytesIsUtf8_thenReturnAxa()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertEquals("AXA", TarUtils.parseName("AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#parseName(byte[], int, int)} with {@code buffer}, {@code offset}, {@code length}.
   * <ul>
   *   <li>When three.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseName(byte[], int, int)}
   */
  @Test
  public void testParseNameWithBufferOffsetLength_whenThree_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", TarUtils.parseName(new byte[]{'A', 'X', TarConstants.LF_OLDNORM, 'X', 'A', 'X', 'A', 'X'}, 2, 3));
  }

  /**
   * Test {@link TarUtils#parseName(byte[], int, int)} with {@code buffer}, {@code offset}, {@code length}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#parseName(byte[], int, int)}
   */
  @Test
  public void testParseNameWithBufferOffsetLength_whenZero_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", TarUtils.parseName(new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'}, 2, 0));
  }

  /**
   * Test {@link TarUtils#formatNameBytes(String, byte[], int, int)} with {@code name}, {@code buf}, {@code offset}, {@code length}.
   * <p>
   * Method under test: {@link TarUtils#formatNameBytes(String, byte[], int, int)}
   */
  @Test
  public void testFormatNameBytesWithNameBufOffsetLength() throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(5, TarUtils.formatNameBytes("Name", buf, 2, 3));
    assertArrayEquals("AXNamXAX".getBytes("UTF-8"), buf);
  }

  /**
   * Test {@link TarUtils#formatNameBytes(String, byte[], int, int)} with {@code name}, {@code buf}, {@code offset}, {@code length}.
   * <p>
   * Method under test: {@link TarUtils#formatNameBytes(String, byte[], int, int)}
   */
  @Test
  public void testFormatNameBytesWithNameBufOffsetLength2() throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(5, TarUtils.formatNameBytes("42", buf, 2, 3));
    assertArrayEquals(new byte[]{'A', 'X', '4', '2', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, buf);
  }

  /**
   * Test {@link TarUtils#formatNameBytes(String, byte[], int, int, ZipEncoding)} with {@code name}, {@code buf}, {@code offset}, {@code length}, {@code encoding}.
   * <p>
   * Method under test: {@link TarUtils#formatNameBytes(String, byte[], int, int, ZipEncoding)}
   */
  @Test
  public void testFormatNameBytesWithNameBufOffsetLengthEncoding() throws IOException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(5, TarUtils.formatNameBytes("Name", buf, 2, 3, TarUtils.DEFAULT_ENCODING));
    assertArrayEquals("AXNamXAX".getBytes("UTF-8"), buf);
  }

  /**
   * Test {@link TarUtils#formatNameBytes(String, byte[], int, int, ZipEncoding)} with {@code name}, {@code buf}, {@code offset}, {@code length}, {@code encoding}.
   * <p>
   * Method under test: {@link TarUtils#formatNameBytes(String, byte[], int, int, ZipEncoding)}
   */
  @Test
  public void testFormatNameBytesWithNameBufOffsetLengthEncoding2() throws IOException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(5, TarUtils.formatNameBytes("42", buf, 2, 3, TarUtils.DEFAULT_ENCODING));
    assertArrayEquals(new byte[]{'A', 'X', '4', '2', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, buf);
  }

  /**
   * Test {@link TarUtils#formatNameBytes(String, byte[], int, int, ZipEncoding)} with {@code name}, {@code buf}, {@code offset}, {@code length}, {@code encoding}.
   * <ul>
   *   <li>When {@link TarUtils#FALLBACK_ENCODING}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatNameBytes(String, byte[], int, int, ZipEncoding)}
   */
  @Test
  public void testFormatNameBytesWithNameBufOffsetLengthEncoding_whenFallback_encoding() throws IOException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(5, TarUtils.formatNameBytes("Name", buf, 2, 3, TarUtils.FALLBACK_ENCODING));
    assertArrayEquals("AXNamXAX".getBytes("UTF-8"), buf);
  }

  /**
   * Test {@link TarUtils#formatNameBytes(String, byte[], int, int, ZipEncoding)} with {@code name}, {@code buf}, {@code offset}, {@code length}, {@code encoding}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatNameBytes(String, byte[], int, int, ZipEncoding)}
   */
  @Test
  public void testFormatNameBytesWithNameBufOffsetLengthEncoding_whenMinusOne_thenReturnOne() throws IOException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(1, TarUtils.formatNameBytes("Name", buf, 2, -1, TarUtils.DEFAULT_ENCODING));
    assertArrayEquals("AXAXAXAX".getBytes("UTF-8"), buf);
  }

  /**
   * Test {@link TarUtils#formatNameBytes(String, byte[], int, int)} with {@code name}, {@code buf}, {@code offset}, {@code length}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatNameBytes(String, byte[], int, int)}
   */
  @Test
  public void testFormatNameBytesWithNameBufOffsetLength_whenMinusOne_thenReturnOne()
      throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(1, TarUtils.formatNameBytes("Name", buf, 2, -1));
    assertArrayEquals("AXAXAXAX".getBytes("UTF-8"), buf);
  }

  /**
   * Test {@link TarUtils#formatUnsignedOctalString(long, byte[], int, int)}.
   * <ul>
   *   <li>Then array of {@code byte} with {@code A} and {@code X} is {@code AX000XAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatUnsignedOctalString(long, byte[], int, int)}
   */
  @Test
  public void testFormatUnsignedOctalString_thenArrayOfByteWithAAndXIsAx000xaxBytesIsUtf8()
      throws UnsupportedEncodingException {
    // Arrange
    byte[] buffer = new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'};

    // Act
    TarUtils.formatUnsignedOctalString(0L, buffer, 2, 3);

    // Assert
    assertArrayEquals("AX000XAX".getBytes("UTF-8"), buffer);
  }

  /**
   * Test {@link TarUtils#formatUnsignedOctalString(long, byte[], int, int)}.
   * <ul>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is {@code AX052XAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatUnsignedOctalString(long, byte[], int, int)}
   */
  @Test
  public void testFormatUnsignedOctalString_thenAxaxaxaxBytesIsUtf8IsAx052xaxBytesIsUtf8()
      throws UnsupportedEncodingException {
    // Arrange
    byte[] buffer = "AXAXAXAX".getBytes("UTF-8");

    // Act
    TarUtils.formatUnsignedOctalString(42L, buffer, 2, 3);

    // Assert
    assertArrayEquals("AX052XAX".getBytes("UTF-8"), buffer);
  }

  /**
   * Test {@link TarUtils#formatUnsignedOctalString(long, byte[], int, int)}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatUnsignedOctalString(long, byte[], int, int)}
   */
  @Test
  public void testFormatUnsignedOctalString_whenMinusOne_thenThrowIllegalArgumentException()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.formatUnsignedOctalString(-1L, "AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#formatOctalBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is array of {@code byte} with {@code A} and {@code X}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatOctalBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatOctalBytes_thenAxaxaxaxBytesIsUtf8IsArrayOfByteWithAAndX() throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(5, TarUtils.formatOctalBytes(2L, buf, 2, 3));
    assertArrayEquals(new byte[]{'A', 'X', '2', ' ', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, buf);
  }

  /**
   * Test {@link TarUtils#formatOctalBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then array of {@code byte} with {@code A} and {@code X}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatOctalBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatOctalBytes_whenA_thenArrayOfByteWithAAndX() {
    // Arrange
    byte[] buf = new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'};

    // Act and Assert
    assertEquals(5, TarUtils.formatOctalBytes(0L, buf, 2, 3));
    assertArrayEquals(new byte[]{'A', 'X', '0', ' ', TarConstants.LF_OLDNORM, 'X', 'A', 'X'}, buf);
  }

  /**
   * Test {@link TarUtils#formatOctalBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatOctalBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatOctalBytes_whenFortyTwo_thenThrowIllegalArgumentException()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.formatOctalBytes(42L, "AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#formatLongOctalBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is {@code AX52 XAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatLongOctalBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatLongOctalBytes_thenAxaxaxaxBytesIsUtf8IsAx52XaxBytesIsUtf8()
      throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(5, TarUtils.formatLongOctalBytes(42L, buf, 2, 3));
    assertArrayEquals("AX52 XAX".getBytes("UTF-8"), buf);
  }

  /**
   * Test {@link TarUtils#formatLongOctalBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then array of {@code byte} with {@code A} and {@code X} is {@code AX00 XAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatLongOctalBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatLongOctalBytes_whenA_thenArrayOfByteWithAAndXIsAx00XaxBytesIsUtf8()
      throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'};

    // Act and Assert
    assertEquals(5, TarUtils.formatLongOctalBytes(0L, buf, 2, 3));
    assertArrayEquals("AX00 XAX".getBytes("UTF-8"), buf);
  }

  /**
   * Test {@link TarUtils#formatLongOctalBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatLongOctalBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatLongOctalBytes_whenMinusOne_thenThrowIllegalArgumentException()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.formatLongOctalBytes(-1L, "AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#formatLongOctalBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is {@code AX01 XAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatLongOctalBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatLongOctalBytes_whenOne_thenAxaxaxaxBytesIsUtf8IsAx01XaxBytesIsUtf8()
      throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(5, TarUtils.formatLongOctalBytes(1L, buf, 2, 3));
    assertArrayEquals("AX01 XAX".getBytes("UTF-8"), buf);
  }

  /**
   * Test {@link TarUtils#formatLongOctalOrBinaryBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is array of {@code byte} with {@code A} and {@code X}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatLongOctalOrBinaryBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatLongOctalOrBinaryBytes_thenAxaxaxaxBytesIsUtf8IsArrayOfByteWithAAndX()
      throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(5, TarUtils.formatLongOctalOrBinaryBytes(-1L, buf, 2, 3));
    assertArrayEquals(new byte[]{'A', 'X', -1, -1, -1, 'X', 'A', 'X'}, buf);
  }

  /**
   * Test {@link TarUtils#formatLongOctalOrBinaryBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is {@code AX00 XAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatLongOctalOrBinaryBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatLongOctalOrBinaryBytes_thenAxaxaxaxBytesIsUtf8IsAx00XaxBytesIsUtf8()
      throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(5, TarUtils.formatLongOctalOrBinaryBytes(0L, buf, 2, 3));
    assertArrayEquals("AX00 XAX".getBytes("UTF-8"), buf);
  }

  /**
   * Test {@link TarUtils#formatLongOctalOrBinaryBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is {@code AX03 XAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatLongOctalOrBinaryBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatLongOctalOrBinaryBytes_thenAxaxaxaxBytesIsUtf8IsAx03XaxBytesIsUtf8()
      throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(5, TarUtils.formatLongOctalOrBinaryBytes(3L, buf, 2, 3));
    assertArrayEquals("AX03 XAX".getBytes("UTF-8"), buf);
  }

  /**
   * Test {@link TarUtils#formatLongOctalOrBinaryBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is {@code AX52 XAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatLongOctalOrBinaryBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatLongOctalOrBinaryBytes_thenAxaxaxaxBytesIsUtf8IsAx52XaxBytesIsUtf8()
      throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(5, TarUtils.formatLongOctalOrBinaryBytes(42L, buf, 2, 3));
    assertArrayEquals("AX52 XAX".getBytes("UTF-8"), buf);
  }

  /**
   * Test {@link TarUtils#formatLongOctalOrBinaryBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>When {@code 8589934592}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatLongOctalOrBinaryBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatLongOctalOrBinaryBytes_when8589934592() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.formatLongOctalOrBinaryBytes(8589934592L, "AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#formatLongOctalOrBinaryBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>When {@link TarConstants#MAXSIZE}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatLongOctalOrBinaryBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatLongOctalOrBinaryBytes_whenMaxsize_thenThrowIllegalArgumentException()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.formatLongOctalOrBinaryBytes(TarConstants.MAXSIZE, "AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#formatCheckSumOctalBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is array of {@code byte} with {@code A} and {@code X}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatCheckSumOctalBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatCheckSumOctalBytes_thenAxaxaxaxBytesIsUtf8IsArrayOfByteWithAAndX()
      throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(5, TarUtils.formatCheckSumOctalBytes(2L, buf, 2, 3));
    assertArrayEquals(new byte[]{'A', 'X', '2', TarConstants.LF_OLDNORM, ' ', 'X', 'A', 'X'}, buf);
  }

  /**
   * Test {@link TarUtils#formatCheckSumOctalBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then array of {@code byte} with {@code A} and {@code X}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatCheckSumOctalBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatCheckSumOctalBytes_whenA_thenArrayOfByteWithAAndX() {
    // Arrange
    byte[] buf = new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'};

    // Act and Assert
    assertEquals(5, TarUtils.formatCheckSumOctalBytes(0L, buf, 2, 3));
    assertArrayEquals(new byte[]{'A', 'X', '0', TarConstants.LF_OLDNORM, ' ', 'X', 'A', 'X'}, buf);
  }

  /**
   * Test {@link TarUtils#formatCheckSumOctalBytes(long, byte[], int, int)}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarUtils#formatCheckSumOctalBytes(long, byte[], int, int)}
   */
  @Test
  public void testFormatCheckSumOctalBytes_whenFortyTwo_thenThrowIllegalArgumentException()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> TarUtils.formatCheckSumOctalBytes(42L, "AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link TarUtils#computeCheckSum(byte[])}.
   * <p>
   * Method under test: {@link TarUtils#computeCheckSum(byte[])}
   */
  @Test
  public void testComputeCheckSum() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertEquals(612L, TarUtils.computeCheckSum("AXAXAXAX".getBytes("UTF-8")));
  }
}
