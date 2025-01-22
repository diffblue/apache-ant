package org.apache.tools.ant;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Set;
import java.util.Vector;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.types.optional.depend.DependScanner;
import org.apache.tools.ant.types.selectors.AndSelector;
import org.apache.tools.ant.types.selectors.FileSelector;
import org.apache.tools.ant.types.selectors.OrSelector;
import org.junit.Test;

public class DirectoryScannerDiffblueTest {
  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code **}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenAsteriskAsterisk() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPatternStart("**", "Str", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code ** /.DS_Store}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenDsStore_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPatternStart("**/.DS_Store", "Str", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenEmptyString() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPatternStart("", "", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenEmptyString2() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPatternStart("", "Str", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenEmptyString3() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPatternStart("Pattern", "", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenFalse_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPatternStart("Pattern", "Pattern", false));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code openvms}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenOpenvms_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPatternStart("Pattern", "openvms", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code openvms}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenOpenvms_thenReturnFalse2() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPatternStart("Pattern", "openvms", false));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenPattern_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPatternStart("Pattern", "Pattern", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code Str}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenStr_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPatternStart("Pattern", "Str", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code **}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenAsteriskAsterisk_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPatternStart("**", "Str"));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code ** /.DS_Store}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenDsStore_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPatternStart("**/.DS_Store", "Str"));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenEmptyString_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPatternStart("", "Str"));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenEmptyString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPatternStart("", ""));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenEmptyString_thenReturnTrue2() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPatternStart("Pattern", ""));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code openvms}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenOpenvms_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPatternStart("Pattern", "openvms"));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenPattern_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPatternStart("Pattern", "Str"));
  }

  /**
   * Test {@link DirectoryScanner#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenPattern_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPatternStart("Pattern", "Pattern"));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code **}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenAsteriskAsterisk_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPath("**", "Str", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code **}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenAsteriskAsterisk_thenReturnTrue2() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPath("**", "", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code ** /.DS_Store}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenDsStore_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPath("**/.DS_Store", "Str", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code .DS_Store}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenDsStore_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPath("**/.DS_Store", ".DS_Store", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenEmptyString_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPath("", "Str", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenEmptyString_thenReturnFalse2() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPath("Pattern", "", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenEmptyString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPath("", "", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenFalse_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPath("Pattern", "Pattern", false));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code openvms}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenOpenvms_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPath("Pattern", "openvms", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code openvms}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenOpenvms_thenReturnFalse2() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPath("Pattern", "openvms", false));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenPattern_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPath("Pattern", "Pattern", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code Str}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenStr_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPath("Pattern", "Str", true));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code **}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenAsteriskAsterisk_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPath("**", "Str"));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code **}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenAsteriskAsterisk_thenReturnTrue2() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPath("**", ""));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code ** /.DS_Store}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenDsStore_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPath("**/.DS_Store", "Str"));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code .DS_Store}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenDsStore_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPath("**/.DS_Store", ".DS_Store"));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenEmptyString_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPath("", "Str"));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenEmptyString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPath("", ""));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code openvms}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenOpenvms_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPath("Pattern", "openvms"));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenPattern_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPath("Pattern", "Str"));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenPattern_thenReturnFalse2() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.matchPath("Pattern", ""));
  }

  /**
   * Test {@link DirectoryScanner#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenPattern_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.matchPath("Pattern", "Pattern"));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code **}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrIsCaseSensitive_whenAsteriskAsterisk_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.match("**", "Str", true));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code **}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrIsCaseSensitive_whenAsteriskAsterisk_thenReturnTrue2() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.match("**", "", true));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code ../}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrIsCaseSensitive_whenDotDotSlash_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.match("../", "Str", true));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code ../}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrIsCaseSensitive_whenDotDotSlash_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.match("../", "../", true));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code ** /.DS_Store}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrIsCaseSensitive_whenDsStore_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.match("**/.DS_Store", "Str", true));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code ** /.DS_Store}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrIsCaseSensitive_whenDsStore_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.match("**/.DS_Store", "**/.DS_Store", true));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrIsCaseSensitive_whenEmptyString_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.match("**/.DS_Store", "", true));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrIsCaseSensitive_whenEmptyString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.match("", "", true));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrIsCaseSensitive_whenFalse_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.match("**/.DS_Store", "Str", false));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrIsCaseSensitive_whenFalse_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.match("**/.DS_Store", "**/.DS_Store", false));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrIsCaseSensitive_whenPattern_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.match("Pattern", "Str", true));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code **}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenAsteriskAsterisk_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.match("**", "Str"));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code **}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenAsteriskAsterisk_thenReturnTrue2() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.match("**", ""));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code ../}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenDotDotSlash_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.match("../", "Str"));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code ../}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenDotDotSlash_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.match("../", "../"));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code ** /.DS_Store}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenDsStore_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.match("**/.DS_Store", "Str"));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code ** /.DS_Store}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenDsStore_thenReturnFalse2() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.match("**/.DS_Store", ""));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code ** /.DS_Store}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenDsStore_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.match("**/.DS_Store", "**/.DS_Store"));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenEmptyString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DirectoryScanner.match("", ""));
  }

  /**
   * Test {@link DirectoryScanner#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenPattern_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.match("Pattern", "Str"));
  }

  /**
   * Test {@link DirectoryScanner#addDefaultExclude(String)}.
   * <ul>
   *   <li>When {@code ** /.DS_Store}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#addDefaultExclude(String)}
   */
  @Test
  public void testAddDefaultExclude_whenDsStore() {
    // Arrange, Act and Assert
    assertFalse(DirectoryScanner.addDefaultExclude("**/.DS_Store"));
  }

  /**
   * Test {@link DirectoryScanner#setBasedir(String)} with {@code String}.
   * <ul>
   *   <li>When {@code Basedir}.</li>
   *   <li>Then {@link DirectoryScanner} (default constructor) Basedir Name is {@code Basedir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#setBasedir(String)}
   */
  @Test
  public void testSetBasedirWithString_whenBasedir_thenDirectoryScannerBasedirNameIsBasedir() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();

    // Act
    directoryScanner.setBasedir("Basedir");

    // Assert
    File basedir = directoryScanner.getBasedir();
    assertEquals("Basedir", basedir.getName());
    assertFalse(basedir.isAbsolute());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link DirectoryScanner#setBasedir(File)}
   *   <li>{@link DirectoryScanner#setCaseSensitive(boolean)}
   *   <li>{@link DirectoryScanner#setErrorOnMissingDir(boolean)}
   *   <li>{@link DirectoryScanner#setFollowSymlinks(boolean)}
   *   <li>{@link DirectoryScanner#setMaxLevelsOfSymlinks(int)}
   *   <li>{@link DirectoryScanner#setSelectors(FileSelector[])}
   *   <li>{@link DirectoryScanner#getBasedir()}
   *   <li>{@link DirectoryScanner#getScannedDirs()}
   *   <li>{@link DirectoryScanner#isCaseSensitive()}
   *   <li>{@link DirectoryScanner#isEverythingIncluded()}
   *   <li>{@link DirectoryScanner#isFollowSymlinks()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    directoryScanner.setBasedir(basedir);
    directoryScanner.setCaseSensitive(true);
    directoryScanner.setErrorOnMissingDir(true);
    directoryScanner.setFollowSymlinks(true);
    directoryScanner.setMaxLevelsOfSymlinks(3);
    directoryScanner.setSelectors(new FileSelector[]{new ScriptSelector()});
    File actualBasedir = directoryScanner.getBasedir();
    Set<String> actualScannedDirs = directoryScanner.getScannedDirs();
    boolean actualIsCaseSensitiveResult = directoryScanner.isCaseSensitive();
    boolean actualIsEverythingIncludedResult = directoryScanner.isEverythingIncluded();
    boolean actualIsFollowSymlinksResult = directoryScanner.isFollowSymlinks();

    // Assert
    assertTrue(actualScannedDirs.isEmpty());
    assertTrue(actualIsCaseSensitiveResult);
    assertTrue(actualIsEverythingIncludedResult);
    assertTrue(actualIsFollowSymlinksResult);
    assertSame(basedir, actualBasedir);
  }

  /**
   * Test {@link DirectoryScanner#setIncludes(String[])}.
   * <p>
   * Method under test: {@link DirectoryScanner#setIncludes(String[])}
   */
  @Test
  public void testSetIncludes() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();

    // Act
    directoryScanner.setIncludes(new String[]{"../"});

    // Assert
    assertArrayEquals(new String[]{"../**"}, directoryScanner.includes);
  }

  /**
   * Test {@link DirectoryScanner#setIncludes(String[])}.
   * <ul>
   *   <li>Then {@link DirectoryScanner} (default constructor) {@link DirectoryScanner#includes} is array of {@link String} with {@code Includes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#setIncludes(String[])}
   */
  @Test
  public void testSetIncludes_thenDirectoryScannerIncludesIsArrayOfStringWithIncludes() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();

    // Act
    directoryScanner.setIncludes(new String[]{"Includes"});

    // Assert
    assertArrayEquals(new String[]{"Includes"}, directoryScanner.includes);
  }

  /**
   * Test {@link DirectoryScanner#setIncludes(String[])}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link DirectoryScanner} (default constructor) {@link DirectoryScanner#includes} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#setIncludes(String[])}
   */
  @Test
  public void testSetIncludes_whenNull_thenDirectoryScannerIncludesIsNull() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();

    // Act
    directoryScanner.setIncludes(null);

    // Assert that nothing has changed
    assertNull(directoryScanner.includes);
  }

  /**
   * Test {@link DirectoryScanner#setExcludes(String[])}.
   * <p>
   * Method under test: {@link DirectoryScanner#setExcludes(String[])}
   */
  @Test
  public void testSetExcludes() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();

    // Act
    directoryScanner.setExcludes(new String[]{"../"});

    // Assert
    assertArrayEquals(new String[]{"../**"}, directoryScanner.excludes);
  }

  /**
   * Test {@link DirectoryScanner#setExcludes(String[])}.
   * <ul>
   *   <li>Then {@link DirectoryScanner} (default constructor) {@link DirectoryScanner#excludes} is array of {@link String} with {@code Excludes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#setExcludes(String[])}
   */
  @Test
  public void testSetExcludes_thenDirectoryScannerExcludesIsArrayOfStringWithExcludes() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();

    // Act
    directoryScanner.setExcludes(new String[]{"Excludes"});

    // Assert
    assertArrayEquals(new String[]{"Excludes"}, directoryScanner.excludes);
  }

  /**
   * Test {@link DirectoryScanner#setExcludes(String[])}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link DirectoryScanner} (default constructor) {@link DirectoryScanner#excludes} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#setExcludes(String[])}
   */
  @Test
  public void testSetExcludes_whenNull_thenDirectoryScannerExcludesIsNull() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();

    // Act
    directoryScanner.setExcludes(null);

    // Assert that nothing has changed
    assertNull(directoryScanner.excludes);
  }

  /**
   * Test {@link DirectoryScanner#addExcludes(String[])}.
   * <p>
   * Method under test: {@link DirectoryScanner#addExcludes(String[])}
   */
  @Test
  public void testAddExcludes() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();

    // Act
    directoryScanner.addExcludes(new String[]{"../"});

    // Assert
    assertArrayEquals(new String[]{"../**"}, directoryScanner.excludes);
  }

  /**
   * Test {@link DirectoryScanner#addExcludes(String[])}.
   * <p>
   * Method under test: {@link DirectoryScanner#addExcludes(String[])}
   */
  @Test
  public void testAddExcludes2() {
    // Arrange
    DependScanner dependScanner = new DependScanner(new DirectoryScanner());

    // Act
    dependScanner.addExcludes(new String[]{null});

    // Assert that nothing has changed
    assertNull(dependScanner.excludes);
  }

  /**
   * Test {@link DirectoryScanner#addExcludes(String[])}.
   * <ul>
   *   <li>Given {@link DirectoryScanner} (default constructor) Excludes is empty array of {@link String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#addExcludes(String[])}
   */
  @Test
  public void testAddExcludes_givenDirectoryScannerExcludesIsEmptyArrayOfString() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.setExcludes(new String[]{});

    // Act
    directoryScanner.addExcludes(new String[]{"Excludes"});

    // Assert
    assertArrayEquals(new String[]{"Excludes"}, directoryScanner.excludes);
  }

  /**
   * Test {@link DirectoryScanner#addExcludes(String[])}.
   * <ul>
   *   <li>Given {@link DirectoryScanner} (default constructor) Excludes is {@code null}.</li>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#addExcludes(String[])}
   */
  @Test
  public void testAddExcludes_givenDirectoryScannerExcludesIsNull_whenNull() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.setExcludes(null);

    // Act
    directoryScanner.addExcludes(null);

    // Assert that nothing has changed
    assertNull(directoryScanner.excludes);
  }

  /**
   * Test {@link DirectoryScanner#addExcludes(String[])}.
   * <ul>
   *   <li>Then {@link DirectoryScanner} (default constructor) {@link DirectoryScanner#excludes} is array of {@link String} with {@code Excludes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#addExcludes(String[])}
   */
  @Test
  public void testAddExcludes_thenDirectoryScannerExcludesIsArrayOfStringWithExcludes() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();

    // Act
    directoryScanner.addExcludes(new String[]{"Excludes"});

    // Assert
    assertArrayEquals(new String[]{"Excludes"}, directoryScanner.excludes);
  }

  /**
   * Test {@link DirectoryScanner#addExcludes(String[])}.
   * <ul>
   *   <li>Then {@link DirectoryScanner} (default constructor) {@link DirectoryScanner#excludes} is array of {@link String} with {@code foo} and {@code Excludes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#addExcludes(String[])}
   */
  @Test
  public void testAddExcludes_thenDirectoryScannerExcludesIsArrayOfStringWithFooAndExcludes() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.setExcludes(new String[]{"foo"});

    // Act
    directoryScanner.addExcludes(new String[]{"Excludes"});

    // Assert
    assertArrayEquals(new String[]{"foo", "Excludes"}, directoryScanner.excludes);
  }

  /**
   * Test {@link DirectoryScanner#addExcludes(String[])}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   *   <li>Then {@link DirectoryScanner} (default constructor) {@link DirectoryScanner#excludes} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#addExcludes(String[])}
   */
  @Test
  public void testAddExcludes_whenEmptyArrayOfString_thenDirectoryScannerExcludesIsNull() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();

    // Act
    directoryScanner.addExcludes(new String[]{});

    // Assert that nothing has changed
    assertNull(directoryScanner.excludes);
  }

  /**
   * Test {@link DirectoryScanner#scan()}.
   * <p>
   * Method under test: {@link DirectoryScanner#scan()}
   */
  @Test
  public void testScan() throws IllegalStateException {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.setIncludes(new String[]{"*", "**"});
    directoryScanner.addExcludes(new String[]{"**"});

    // Act
    directoryScanner.scan();

    // Assert
    Vector<String> stringList = directoryScanner.dirsExcluded;
    assertEquals(1, stringList.size());
    assertEquals("", stringList.get(0));
    assertFalse(directoryScanner.isEverythingIncluded());
    assertTrue(directoryScanner.getScannedDirs().isEmpty());
  }

  /**
   * Test {@link DirectoryScanner#scan()}.
   * <ul>
   *   <li>Given {@link DirectoryScanner} (default constructor) Includes is array of {@link String} with {@code *}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#scan()}
   */
  @Test
  public void testScan_givenDirectoryScannerIncludesIsArrayOfStringWithAsterisk() throws IllegalStateException {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.setIncludes(new String[]{"*"});
    directoryScanner.addExcludes(new String[]{"**"});

    // Act
    directoryScanner.scan();

    // Assert
    Vector<String> stringList = directoryScanner.dirsNotIncluded;
    assertEquals(1, stringList.size());
    assertEquals("", stringList.get(0));
    assertTrue(directoryScanner.dirsExcluded.isEmpty());
  }

  /**
   * Test {@link DirectoryScanner#scan()}.
   * <ul>
   *   <li>Given {@link DirectoryScanner} (default constructor) Includes is array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#scan()}
   */
  @Test
  public void testScan_givenDirectoryScannerIncludesIsArrayOfStringWithEmptyString() throws IllegalStateException {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.setIncludes(new String[]{""});
    directoryScanner.addExcludes(new String[]{"**"});

    // Act
    directoryScanner.scan();

    // Assert
    Vector<String> stringList = directoryScanner.dirsExcluded;
    assertEquals(1, stringList.size());
    assertEquals("", stringList.get(0));
    assertFalse(directoryScanner.isEverythingIncluded());
    assertTrue(directoryScanner.getScannedDirs().isEmpty());
  }

  /**
   * Test {@link DirectoryScanner#scan()}.
   * <ul>
   *   <li>Given {@link DirectoryScanner} (default constructor) Includes is array of {@link String} with {@code Includes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#scan()}
   */
  @Test
  public void testScan_givenDirectoryScannerIncludesIsArrayOfStringWithIncludes() throws IllegalStateException {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.setIncludes(new String[]{"Includes"});
    directoryScanner.addExcludes(new String[]{"**"});

    // Act
    directoryScanner.scan();

    // Assert
    Vector<String> stringList = directoryScanner.dirsNotIncluded;
    assertEquals(1, stringList.size());
    assertEquals("", stringList.get(0));
    assertTrue(directoryScanner.dirsExcluded.isEmpty());
  }

  /**
   * Test {@link DirectoryScanner#scan()}.
   * <ul>
   *   <li>Given {@link DirectoryScanner} (default constructor) Includes is array of {@link String} with {@code ?}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#scan()}
   */
  @Test
  public void testScan_givenDirectoryScannerIncludesIsArrayOfStringWithQuestionMark() throws IllegalStateException {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.setIncludes(new String[]{"?"});
    directoryScanner.addExcludes(new String[]{"**"});

    // Act
    directoryScanner.scan();

    // Assert
    Vector<String> stringList = directoryScanner.dirsNotIncluded;
    assertEquals(1, stringList.size());
    assertEquals("", stringList.get(0));
    assertTrue(directoryScanner.dirsExcluded.isEmpty());
  }

  /**
   * Test {@link DirectoryScanner#scan()}.
   * <ul>
   *   <li>Given {@link DirectoryScanner} (default constructor).</li>
   *   <li>Then array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#scan()}
   */
  @Test
  public void testScan_givenDirectoryScanner_thenArrayLengthIsZero() throws IllegalStateException {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();

    // Act
    directoryScanner.scan();

    // Assert
    assertEquals(0, directoryScanner.excludes.length);
    assertTrue(directoryScanner.getScannedDirs().isEmpty());
    assertArrayEquals(new String[]{"**"}, directoryScanner.includes);
  }

  /**
   * Test {@link DirectoryScanner#scan()}.
   * <ul>
   *   <li>Then {@link DirectoryScanner} (default constructor) {@link DirectoryScanner#includes} is array of {@link String} with {@code **}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#scan()}
   */
  @Test
  public void testScan_thenDirectoryScannerIncludesIsArrayOfStringWithAsteriskAsterisk() throws IllegalStateException {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"**"});

    // Act
    directoryScanner.scan();

    // Assert
    assertTrue(directoryScanner.dirsExcluded.isEmpty());
    assertArrayEquals(new String[]{"**"}, directoryScanner.includes);
  }

  /**
   * Test {@link DirectoryScanner#scan()}.
   * <ul>
   *   <li>Then not {@link DirectoryScanner} (default constructor) EverythingIncluded.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#scan()}
   */
  @Test
  public void testScan_thenNotDirectoryScannerEverythingIncluded() throws IllegalStateException {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.setIncludes(new String[]{"**"});
    directoryScanner.addExcludes(new String[]{"**"});

    // Act
    directoryScanner.scan();

    // Assert
    Vector<String> stringList = directoryScanner.dirsExcluded;
    assertEquals(1, stringList.size());
    assertEquals("", stringList.get(0));
    assertFalse(directoryScanner.isEverythingIncluded());
    assertTrue(directoryScanner.getScannedDirs().isEmpty());
  }

  /**
   * Test {@link DirectoryScanner#scan()}.
   * <ul>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#scan()}
   */
  @Test
  public void testScan_thenThrowIllegalStateException() throws IllegalStateException {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.setBasedir(Paths.get(System.getProperty("java.io.tmpdir"), "**").toFile());
    directoryScanner.addExcludes(new String[]{"**"});

    // Act and Assert
    assertThrows(IllegalStateException.class, () -> directoryScanner.scan());
  }

  /**
   * Test {@link DirectoryScanner#clearResults()}.
   * <ul>
   *   <li>Given {@link DirectoryScanner} (default constructor).</li>
   *   <li>Then not {@link DirectoryScanner} (default constructor) EverythingIncluded.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#clearResults()}
   */
  @Test
  public void testClearResults_givenDirectoryScanner_thenNotDirectoryScannerEverythingIncluded() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();

    // Act
    directoryScanner.clearResults();

    // Assert
    assertEquals(0, directoryScanner.getIncludedDirsCount());
    assertEquals(0, directoryScanner.getIncludedFilesCount());
    assertEquals(0, directoryScanner.getIncludedDirectories().length);
    assertEquals(0, directoryScanner.getIncludedFiles().length);
    assertFalse(directoryScanner.isEverythingIncluded());
    assertTrue(directoryScanner.dirsDeselected.isEmpty());
    assertTrue(directoryScanner.dirsExcluded.isEmpty());
    assertTrue(directoryScanner.dirsIncluded.isEmpty());
    assertTrue(directoryScanner.dirsNotIncluded.isEmpty());
    assertTrue(directoryScanner.filesDeselected.isEmpty());
    assertTrue(directoryScanner.filesExcluded.isEmpty());
    assertTrue(directoryScanner.filesIncluded.isEmpty());
    assertTrue(directoryScanner.filesNotIncluded.isEmpty());
  }

  /**
   * Test {@link DirectoryScanner#clearResults()}.
   * <ul>
   *   <li>Then {@link DirectoryScanner} (default constructor) EverythingIncluded.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#clearResults()}
   */
  @Test
  public void testClearResults_thenDirectoryScannerEverythingIncluded() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.setBasedir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    directoryScanner.clearResults();

    // Assert
    assertEquals(0, directoryScanner.getIncludedDirsCount());
    assertEquals(0, directoryScanner.getIncludedFilesCount());
    assertEquals(0, directoryScanner.getIncludedDirectories().length);
    assertEquals(0, directoryScanner.getIncludedFiles().length);
    assertTrue(directoryScanner.dirsDeselected.isEmpty());
    assertTrue(directoryScanner.dirsExcluded.isEmpty());
    assertTrue(directoryScanner.dirsIncluded.isEmpty());
    assertTrue(directoryScanner.dirsNotIncluded.isEmpty());
    assertTrue(directoryScanner.filesDeselected.isEmpty());
    assertTrue(directoryScanner.filesExcluded.isEmpty());
    assertTrue(directoryScanner.filesIncluded.isEmpty());
    assertTrue(directoryScanner.filesNotIncluded.isEmpty());
    assertTrue(directoryScanner.isEverythingIncluded());
  }

  /**
   * Test {@link DirectoryScanner#scandir(File, String, boolean)} with {@code dir}, {@code vpath}, {@code fast}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#scandir(File, String, boolean)}
   */
  @Test
  public void testScandirWithDirVpathFast_whenNull_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new DirectoryScanner()).scandir(null, "", true));
  }

  /**
   * Test {@link DirectoryScanner#isIncluded(String)} with {@code name}.
   * <p>
   * Method under test: {@link DirectoryScanner#isIncluded(String)}
   */
  @Test
  public void testIsIncludedWithName() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"?", "?"});

    // Act and Assert
    assertFalse(directoryScanner.isIncluded("Name"));
  }

  /**
   * Test {@link DirectoryScanner#isIncluded(String)} with {@code name}.
   * <p>
   * Method under test: {@link DirectoryScanner#isIncluded(String)}
   */
  @Test
  public void testIsIncludedWithName2() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"**", "?"});

    // Act and Assert
    assertTrue(directoryScanner.isIncluded("Name"));
  }

  /**
   * Test {@link DirectoryScanner#isIncluded(String)} with {@code name}.
   * <p>
   * Method under test: {@link DirectoryScanner#isIncluded(String)}
   */
  @Test
  public void testIsIncludedWithName3() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"**/.DS_Store", "?"});

    // Act and Assert
    assertFalse(directoryScanner.isIncluded("Name"));
  }

  /**
   * Test {@link DirectoryScanner#isIncluded(String)} with {@code name}.
   * <p>
   * Method under test: {@link DirectoryScanner#isIncluded(String)}
   */
  @Test
  public void testIsIncludedWithName4() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"**", "?"});

    // Act and Assert
    assertTrue(directoryScanner.isIncluded(""));
  }

  /**
   * Test {@link DirectoryScanner#isIncluded(String)} with {@code name}.
   * <p>
   * Method under test: {@link DirectoryScanner#isIncluded(String)}
   */
  @Test
  public void testIsIncludedWithName5() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"", "?"});

    // Act and Assert
    assertTrue(directoryScanner.isIncluded(""));
  }

  /**
   * Test {@link DirectoryScanner#isIncluded(String)} with {@code name}.
   * <ul>
   *   <li>Given {@link DirectoryScanner} (default constructor) Includes is empty array of {@link String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isIncluded(String)}
   */
  @Test
  public void testIsIncludedWithName_givenDirectoryScannerIncludesIsEmptyArrayOfString() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{});

    // Act and Assert
    assertFalse(directoryScanner.isIncluded("Name"));
  }

  /**
   * Test {@link DirectoryScanner#isIncluded(String)} with {@code name}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isIncluded(String)}
   */
  @Test
  public void testIsIncludedWithName_thenReturnTrue() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertTrue(directoryScanner.isIncluded("Name"));
  }

  /**
   * Test {@link DirectoryScanner#isIncluded(String)} with {@code name}.
   * <ul>
   *   <li>When {@code *}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isIncluded(String)}
   */
  @Test
  public void testIsIncludedWithName_whenAsterisk() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"?", "?"});

    // Act and Assert
    assertTrue(directoryScanner.isIncluded("*"));
  }

  /**
   * Test {@link DirectoryScanner#isIncluded(String)} with {@code name}.
   * <ul>
   *   <li>When {@code .DS_Store}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isIncluded(String)}
   */
  @Test
  public void testIsIncludedWithName_whenDsStore() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"**/.DS_Store", "?"});

    // Act and Assert
    assertTrue(directoryScanner.isIncluded(".DS_Store"));
  }

  /**
   * Test {@link DirectoryScanner#isIncluded(String)} with {@code name}.
   * <ul>
   *   <li>When {@code ** /.DS_Store}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isIncluded(String)}
   */
  @Test
  public void testIsIncludedWithName_whenDsStore_thenReturnFalse() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(directoryScanner.isIncluded("**/.DS_Store"));
  }

  /**
   * Test {@link DirectoryScanner#isIncluded(String)} with {@code name}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isIncluded(String)}
   */
  @Test
  public void testIsIncludedWithName_whenEmptyString_thenReturnFalse() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(directoryScanner.isIncluded(""));
  }

  /**
   * Test {@link DirectoryScanner#isExcluded(String)} with {@code String}.
   * <p>
   * Method under test: {@link DirectoryScanner#isExcluded(String)}
   */
  @Test
  public void testIsExcludedWithString() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"?"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(directoryScanner.isExcluded("Name"));
  }

  /**
   * Test {@link DirectoryScanner#isExcluded(String)} with {@code String}.
   * <p>
   * Method under test: {@link DirectoryScanner#isExcluded(String)}
   */
  @Test
  public void testIsExcludedWithString2() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"**"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertTrue(directoryScanner.isExcluded("Name"));
  }

  /**
   * Test {@link DirectoryScanner#isExcluded(String)} with {@code String}.
   * <p>
   * Method under test: {@link DirectoryScanner#isExcluded(String)}
   */
  @Test
  public void testIsExcludedWithString3() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"Excludes"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(directoryScanner.isExcluded("Name"));
  }

  /**
   * Test {@link DirectoryScanner#isExcluded(String)} with {@code String}.
   * <p>
   * Method under test: {@link DirectoryScanner#isExcluded(String)}
   */
  @Test
  public void testIsExcludedWithString4() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"**/.DS_Store"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(directoryScanner.isExcluded("Name"));
  }

  /**
   * Test {@link DirectoryScanner#isExcluded(String)} with {@code String}.
   * <p>
   * Method under test: {@link DirectoryScanner#isExcluded(String)}
   */
  @Test
  public void testIsExcludedWithString5() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"**/.DS_Store"});
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertTrue(directoryScanner.isExcluded("Name"));
  }

  /**
   * Test {@link DirectoryScanner#isExcluded(String)} with {@code String}.
   * <p>
   * Method under test: {@link DirectoryScanner#isExcluded(String)}
   */
  @Test
  public void testIsExcludedWithString6() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"**"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertTrue(directoryScanner.isExcluded(""));
  }

  /**
   * Test {@link DirectoryScanner#isExcluded(String)} with {@code String}.
   * <p>
   * Method under test: {@link DirectoryScanner#isExcluded(String)}
   */
  @Test
  public void testIsExcludedWithString7() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{""});
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertTrue(directoryScanner.isExcluded(""));
  }

  /**
   * Test {@link DirectoryScanner#isExcluded(String)} with {@code String}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isExcluded(String)}
   */
  @Test
  public void testIsExcludedWithString_thenReturnTrue() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertTrue(directoryScanner.isExcluded("Name"));
  }

  /**
   * Test {@link DirectoryScanner#isExcluded(String)} with {@code String}.
   * <ul>
   *   <li>When {@code *}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isExcluded(String)}
   */
  @Test
  public void testIsExcludedWithString_whenAsterisk() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"?"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertTrue(directoryScanner.isExcluded("*"));
  }

  /**
   * Test {@link DirectoryScanner#isExcluded(String)} with {@code String}.
   * <ul>
   *   <li>When {@code .DS_Store}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isExcluded(String)}
   */
  @Test
  public void testIsExcludedWithString_whenDsStore() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"**/.DS_Store"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertTrue(directoryScanner.isExcluded(".DS_Store"));
  }

  /**
   * Test {@link DirectoryScanner#isExcluded(String)} with {@code String}.
   * <ul>
   *   <li>When {@code ** /.DS_Store}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isExcluded(String)}
   */
  @Test
  public void testIsExcludedWithString_whenDsStore2() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"../"});
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(directoryScanner.isExcluded("**/.DS_Store"));
  }

  /**
   * Test {@link DirectoryScanner#isExcluded(String)} with {@code String}.
   * <ul>
   *   <li>When {@code ** /.DS_Store}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isExcluded(String)}
   */
  @Test
  public void testIsExcludedWithString_whenDsStore_thenReturnFalse() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(directoryScanner.isExcluded("**/.DS_Store"));
  }

  /**
   * Test {@link DirectoryScanner#isExcluded(String)} with {@code String}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isExcluded(String)}
   */
  @Test
  public void testIsExcludedWithString_whenEmptyString_thenReturnFalse() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.addExcludes(new String[]{"*"});
    directoryScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(directoryScanner.isExcluded(""));
  }

  /**
   * Test {@link DirectoryScanner#isSelected(String, File)}.
   * <p>
   * Method under test: {@link DirectoryScanner#isSelected(String, File)}
   */
  @Test
  public void testIsSelected() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.setSelectors(new FileSelector[]{new AndSelector()});

    // Act and Assert
    assertTrue(
        directoryScanner.isSelected("Name", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DirectoryScanner#isSelected(String, File)}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor) addOr {@link OrSelector} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isSelected(String, File)}
   */
  @Test
  public void testIsSelected_givenAndSelectorAddOrOrSelector_thenReturnFalse() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.addOr(new OrSelector());

    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.setSelectors(new FileSelector[]{andSelector});

    // Act and Assert
    assertFalse(
        directoryScanner.isSelected("Name", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DirectoryScanner#isSelected(String, File)}.
   * <ul>
   *   <li>Given {@link DirectoryScanner} (default constructor) Selectors is empty array of {@link FileSelector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isSelected(String, File)}
   */
  @Test
  public void testIsSelected_givenDirectoryScannerSelectorsIsEmptyArrayOfFileSelector() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();
    directoryScanner.setSelectors(new FileSelector[]{});

    // Act and Assert
    assertTrue(
        directoryScanner.isSelected("Name", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DirectoryScanner#isSelected(String, File)}.
   * <ul>
   *   <li>Given {@link DirectoryScanner} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryScanner#isSelected(String, File)}
   */
  @Test
  public void testIsSelected_givenDirectoryScanner_thenReturnTrue() {
    // Arrange
    DirectoryScanner directoryScanner = new DirectoryScanner();

    // Act and Assert
    assertTrue(
        directoryScanner.isSelected("Name", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DirectoryScanner#getIncludedFiles()}.
   * <p>
   * Method under test: {@link DirectoryScanner#getIncludedFiles()}
   */
  @Test
  public void testGetIncludedFiles() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new DirectoryScanner()).getIncludedFiles());
  }

  /**
   * Test {@link DirectoryScanner#getIncludedFilesCount()}.
   * <p>
   * Method under test: {@link DirectoryScanner#getIncludedFilesCount()}
   */
  @Test
  public void testGetIncludedFilesCount() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new DirectoryScanner()).getIncludedFilesCount());
  }

  /**
   * Test {@link DirectoryScanner#getIncludedDirectories()}.
   * <p>
   * Method under test: {@link DirectoryScanner#getIncludedDirectories()}
   */
  @Test
  public void testGetIncludedDirectories() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new DirectoryScanner()).getIncludedDirectories());
  }

  /**
   * Test {@link DirectoryScanner#getIncludedDirsCount()}.
   * <p>
   * Method under test: {@link DirectoryScanner#getIncludedDirsCount()}
   */
  @Test
  public void testGetIncludedDirsCount() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new DirectoryScanner()).getIncludedDirsCount());
  }

  /**
   * Test {@link DirectoryScanner#getNotFollowedSymlinks()}.
   * <p>
   * Method under test: {@link DirectoryScanner#getNotFollowedSymlinks()}
   */
  @Test
  public void testGetNotFollowedSymlinks() {
    // Arrange, Act and Assert
    assertEquals(0, (new DirectoryScanner()).getNotFollowedSymlinks().length);
  }

  /**
   * Test new {@link DirectoryScanner} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link DirectoryScanner}
   */
  @Test
  public void testNewDirectoryScanner() {
    // Arrange and Act
    DirectoryScanner actualDirectoryScanner = new DirectoryScanner();

    // Assert
    assertNull(actualDirectoryScanner.excludes);
    assertNull(actualDirectoryScanner.includes);
    assertNull(actualDirectoryScanner.selectors);
    assertNull(actualDirectoryScanner.getBasedir());
    assertNull(actualDirectoryScanner.dirsDeselected);
    assertNull(actualDirectoryScanner.dirsExcluded);
    assertNull(actualDirectoryScanner.dirsIncluded);
    assertNull(actualDirectoryScanner.dirsNotIncluded);
    assertNull(actualDirectoryScanner.filesDeselected);
    assertNull(actualDirectoryScanner.filesExcluded);
    assertNull(actualDirectoryScanner.filesIncluded);
    assertNull(actualDirectoryScanner.filesNotIncluded);
    assertEquals(0, actualDirectoryScanner.getNotFollowedSymlinks().length);
    assertFalse(actualDirectoryScanner.haveSlowResults);
    assertTrue(actualDirectoryScanner.getScannedDirs().isEmpty());
    assertTrue(actualDirectoryScanner.isCaseSensitive());
    assertTrue(actualDirectoryScanner.isEverythingIncluded());
    assertTrue(actualDirectoryScanner.isFollowSymlinks());
    assertTrue(actualDirectoryScanner.errorOnMissingDir);
  }
}
