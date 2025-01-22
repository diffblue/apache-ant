package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Vector;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaConstantResource;
import org.apache.tools.ant.types.resources.MappedResource;
import org.junit.Test;

public class SelectorUtilsDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Method under test: {@link SelectorUtils#getInstance()}
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    SelectorUtils actualInstance = SelectorUtils.getInstance();

    // Assert
    assertSame(actualInstance, actualInstance.getInstance());
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String[], String[], boolean)} with {@code patDirs}, {@code strDirs}, {@code isCaseSensitive}.
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String[], String[], boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatDirsStrDirsIsCaseSensitive() {
    // Arrange, Act and Assert
    assertTrue(
        SelectorUtils.matchPatternStart(new String[]{SelectorUtils.DEEP_TREE_MATCH}, new String[]{"Str Dirs"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String[], String[], boolean)} with {@code patDirs}, {@code strDirs}, {@code isCaseSensitive}.
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String[], String[], boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatDirsStrDirsIsCaseSensitive2() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart(new String[]{"*"}, new String[]{"Str Dirs"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String[], String[], boolean)} with {@code patDirs}, {@code strDirs}, {@code isCaseSensitive}.
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String[], String[], boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatDirsStrDirsIsCaseSensitive3() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart(new String[]{"Pat Dirs"}, new String[]{"Pat Dirs"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String[], String[], boolean)} with {@code patDirs}, {@code strDirs}, {@code isCaseSensitive}.
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String[], String[], boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatDirsStrDirsIsCaseSensitive4() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart(new String[]{"?"}, new String[]{"*"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String[], String[], boolean)} with {@code patDirs}, {@code strDirs}, {@code isCaseSensitive}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String[], String[], boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatDirsStrDirsIsCaseSensitive_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPatternStart(new String[]{"Pat Dirs"}, new String[]{"Str Dirs"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String[], String[], boolean)} with {@code patDirs}, {@code strDirs}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When array of {@link String} with {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String[], String[], boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatDirsStrDirsIsCaseSensitive_whenArrayOfStringWith42() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPatternStart(new String[]{"42"}, new String[]{"Str Dirs"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String[], String[], boolean)} with {@code patDirs}, {@code strDirs}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String[], String[], boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatDirsStrDirsIsCaseSensitive_whenEmptyArrayOfString() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPatternStart(new String[]{}, new String[]{"Str Dirs"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String[], String[], boolean)} with {@code patDirs}, {@code strDirs}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String[], String[], boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatDirsStrDirsIsCaseSensitive_whenEmptyArrayOfString2() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart(new String[]{"Pat Dirs"}, new String[]{}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String[], String[], boolean)} with {@code patDirs}, {@code strDirs}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String[], String[], boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatDirsStrDirsIsCaseSensitive_whenFalse_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPatternStart(new String[]{"Pat Dirs"}, new String[]{"Str Dirs"}, false));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String[], String[], boolean)} with {@code patDirs}, {@code strDirs}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String[], String[], boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatDirsStrDirsIsCaseSensitive_whenFalse_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart(new String[]{"Pat Dirs"}, new String[]{"Pat Dirs"}, false));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_when42_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPatternStart("42", SelectorUtils.DEEP_TREE_MATCH, true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_when42_thenReturnFalse2() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPatternStart("42", SelectorUtils.DEEP_TREE_MATCH, false));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code *}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenAsterisk_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart("*", "Str", true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@link SelectorUtils#DEEP_TREE_MATCH}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenDeep_tree_match() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart(SelectorUtils.DEEP_TREE_MATCH, "Str", true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenEmptyString() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart("", "", true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenEmptyString2() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPatternStart("", "Str", true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenEmptyString3() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart("Pattern", "", true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenFalse_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart("Pattern", "Pattern", false));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenPattern_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPatternStart("Pattern", "Str", true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenPattern_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart("Pattern", "Pattern", true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code ?}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String, boolean)}
   */
  @Test
  public void testMatchPatternStartWithPatternStrIsCaseSensitive_whenQuestionMark() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart("?", "*", true));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_when42_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPatternStart("42", SelectorUtils.DEEP_TREE_MATCH));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code *}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenAsterisk_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart("*", "Str"));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@link SelectorUtils#DEEP_TREE_MATCH}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenDeep_tree_match_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart(SelectorUtils.DEEP_TREE_MATCH, "Str"));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenEmptyString_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPatternStart("", "Str"));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenEmptyString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart("", ""));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenEmptyString_thenReturnTrue2() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart("Pattern", ""));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenPattern_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPatternStart("Pattern", "Str"));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenPattern_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart("Pattern", "Pattern"));
  }

  /**
   * Test {@link SelectorUtils#matchPatternStart(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code ?}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPatternStart(String, String)}
   */
  @Test
  public void testMatchPatternStartWithPatternStr_whenQuestionMark_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPatternStart("?", "*"));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_when42_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPath("42", SelectorUtils.DEEP_TREE_MATCH, true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_when42_thenReturnFalse2() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPath("42", SelectorUtils.DEEP_TREE_MATCH, false));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code *}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenAsterisk_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath("*", "Str", true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@link SelectorUtils#DEEP_TREE_MATCH}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenDeep_tree_match_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath(SelectorUtils.DEEP_TREE_MATCH, "Str", true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenEmptyString_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPath("", "Str", true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenEmptyString_thenReturnFalse2() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPath("Pattern", "", true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenEmptyString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath(SelectorUtils.DEEP_TREE_MATCH, "", true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenFalse_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath("Pattern", "Pattern", false));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenPattern_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPath("Pattern", "Str", true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenPattern_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath("Pattern", "Pattern", true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String, boolean)} with {@code pattern}, {@code str}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code ?}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String, boolean)}
   */
  @Test
  public void testMatchPathWithPatternStrIsCaseSensitive_whenQuestionMark_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath("?", "*", true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_when42_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPath("42", SelectorUtils.DEEP_TREE_MATCH));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code *}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenAsterisk_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath("*", "Str"));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@link SelectorUtils#DEEP_TREE_MATCH}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenDeep_tree_match_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath(SelectorUtils.DEEP_TREE_MATCH, "Str"));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@link SelectorUtils#DEEP_TREE_MATCH}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenDeep_tree_match_thenReturnTrue2() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath(SelectorUtils.DEEP_TREE_MATCH, ""));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenEmptyString_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPath("", "Str"));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenEmptyString_thenReturnFalse2() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPath("Pattern", ""));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenPattern_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPath("Pattern", "Str"));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenPattern_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath("Pattern", "Pattern"));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code ?}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String, String)}
   */
  @Test
  public void testMatchPathWithPatternStr_whenQuestionMark_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath("?", "*"));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String[], String[], boolean)} with {@code tokenizedPattern}, {@code strDirs}, {@code isCaseSensitive}.
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String[], String[], boolean)}
   */
  @Test
  public void testMatchPathWithTokenizedPatternStrDirsIsCaseSensitive() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPath(new String[]{"ABC123"}, new String[]{"Str Dirs"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String[], String[], boolean)} with {@code tokenizedPattern}, {@code strDirs}, {@code isCaseSensitive}.
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String[], String[], boolean)}
   */
  @Test
  public void testMatchPathWithTokenizedPatternStrDirsIsCaseSensitive2() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath(new String[]{SelectorUtils.DEEP_TREE_MATCH}, new String[]{"Str Dirs"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String[], String[], boolean)} with {@code tokenizedPattern}, {@code strDirs}, {@code isCaseSensitive}.
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String[], String[], boolean)}
   */
  @Test
  public void testMatchPathWithTokenizedPatternStrDirsIsCaseSensitive3() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath(new String[]{"*"}, new String[]{"Str Dirs"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String[], String[], boolean)} with {@code tokenizedPattern}, {@code strDirs}, {@code isCaseSensitive}.
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String[], String[], boolean)}
   */
  @Test
  public void testMatchPathWithTokenizedPatternStrDirsIsCaseSensitive4() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath(new String[]{"ABC123"}, new String[]{"ABC123"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String[], String[], boolean)} with {@code tokenizedPattern}, {@code strDirs}, {@code isCaseSensitive}.
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String[], String[], boolean)}
   */
  @Test
  public void testMatchPathWithTokenizedPatternStrDirsIsCaseSensitive5() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath(new String[]{"?"}, new String[]{"*"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String[], String[], boolean)} with {@code tokenizedPattern}, {@code strDirs}, {@code isCaseSensitive}.
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String[], String[], boolean)}
   */
  @Test
  public void testMatchPathWithTokenizedPatternStrDirsIsCaseSensitive6() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath(new String[]{SelectorUtils.DEEP_TREE_MATCH, SelectorUtils.DEEP_TREE_MATCH},
        new String[]{"Str Dirs"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String[], String[], boolean)} with {@code tokenizedPattern}, {@code strDirs}, {@code isCaseSensitive}.
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String[], String[], boolean)}
   */
  @Test
  public void testMatchPathWithTokenizedPatternStrDirsIsCaseSensitive7() {
    // Arrange, Act and Assert
    assertFalse(
        SelectorUtils.matchPath(new String[]{SelectorUtils.DEEP_TREE_MATCH, "ABC123"}, new String[]{"Str Dirs"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String[], String[], boolean)} with {@code tokenizedPattern}, {@code strDirs}, {@code isCaseSensitive}.
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String[], String[], boolean)}
   */
  @Test
  public void testMatchPathWithTokenizedPatternStrDirsIsCaseSensitive8() {
    // Arrange, Act and Assert
    assertTrue(
        SelectorUtils.matchPath(new String[]{SelectorUtils.DEEP_TREE_MATCH, "*"}, new String[]{"Str Dirs"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String[], String[], boolean)} with {@code tokenizedPattern}, {@code strDirs}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When array of {@link String} with {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String[], String[], boolean)}
   */
  @Test
  public void testMatchPathWithTokenizedPatternStrDirsIsCaseSensitive_whenArrayOfStringWith42() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPath(new String[]{"42"}, new String[]{SelectorUtils.DEEP_TREE_MATCH}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String[], String[], boolean)} with {@code tokenizedPattern}, {@code strDirs}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When array of {@link String} with {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String[], String[], boolean)}
   */
  @Test
  public void testMatchPathWithTokenizedPatternStrDirsIsCaseSensitive_whenArrayOfStringWith422() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPath(new String[]{"42"}, new String[]{SelectorUtils.DEEP_TREE_MATCH}, false));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String[], String[], boolean)} with {@code tokenizedPattern}, {@code strDirs}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String[], String[], boolean)}
   */
  @Test
  public void testMatchPathWithTokenizedPatternStrDirsIsCaseSensitive_whenEmptyArrayOfString() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPath(new String[]{}, new String[]{"Str Dirs"}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String[], String[], boolean)} with {@code tokenizedPattern}, {@code strDirs}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String[], String[], boolean)}
   */
  @Test
  public void testMatchPathWithTokenizedPatternStrDirsIsCaseSensitive_whenEmptyArrayOfString2() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.matchPath(new String[]{"ABC123"}, new String[]{}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String[], String[], boolean)} with {@code tokenizedPattern}, {@code strDirs}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String[], String[], boolean)}
   */
  @Test
  public void testMatchPathWithTokenizedPatternStrDirsIsCaseSensitive_whenEmptyArrayOfString3() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath(new String[]{SelectorUtils.DEEP_TREE_MATCH}, new String[]{}, true));
  }

  /**
   * Test {@link SelectorUtils#matchPath(String[], String[], boolean)} with {@code tokenizedPattern}, {@code strDirs}, {@code isCaseSensitive}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#matchPath(String[], String[], boolean)}
   */
  @Test
  public void testMatchPathWithTokenizedPatternStrDirsIsCaseSensitive_whenFalse_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.matchPath(new String[]{"ABC123"}, new String[]{"ABC123"}, false));
  }

  /**
   * Test {@link SelectorUtils#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code caseSensitive}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrCaseSensitive_when42_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.match("42", SelectorUtils.DEEP_TREE_MATCH, true));
  }

  /**
   * Test {@link SelectorUtils#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code caseSensitive}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrCaseSensitive_when42_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.match("42", "42", true));
  }

  /**
   * Test {@link SelectorUtils#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code caseSensitive}.
   * <ul>
   *   <li>When {@code *}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrCaseSensitive_whenAsterisk_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.match("*", "Str", true));
  }

  /**
   * Test {@link SelectorUtils#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code caseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrCaseSensitive_whenEmptyString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.match(SelectorUtils.DEEP_TREE_MATCH, "", true));
  }

  /**
   * Test {@link SelectorUtils#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code caseSensitive}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrCaseSensitive_whenEmptyString_thenReturnTrue2() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.match("", "", true));
  }

  /**
   * Test {@link SelectorUtils#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code caseSensitive}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrCaseSensitive_whenFalse_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.match("42", SelectorUtils.DEEP_TREE_MATCH, false));
  }

  /**
   * Test {@link SelectorUtils#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code caseSensitive}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrCaseSensitive_whenFalse_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.match("42", "42", false));
  }

  /**
   * Test {@link SelectorUtils#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code caseSensitive}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrCaseSensitive_whenPattern_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.match("Pattern", "Str", true));
  }

  /**
   * Test {@link SelectorUtils#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code caseSensitive}.
   * <ul>
   *   <li>When {@code ?}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrCaseSensitive_whenQuestionMark_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.match("?", "*", true));
  }

  /**
   * Test {@link SelectorUtils#match(String, String, boolean)} with {@code pattern}, {@code str}, {@code caseSensitive}.
   * <ul>
   *   <li>When {@code Str}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String, boolean)}
   */
  @Test
  public void testMatchWithPatternStrCaseSensitive_whenStr_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.match(SelectorUtils.DEEP_TREE_MATCH, "Str", true));
  }

  /**
   * Test {@link SelectorUtils#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_when42_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.match("42", SelectorUtils.DEEP_TREE_MATCH));
  }

  /**
   * Test {@link SelectorUtils#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_when42_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.match("42", "42"));
  }

  /**
   * Test {@link SelectorUtils#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code *}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenAsterisk_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.match("*", "Str"));
  }

  /**
   * Test {@link SelectorUtils#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@link SelectorUtils#DEEP_TREE_MATCH}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenDeep_tree_match_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.match(SelectorUtils.DEEP_TREE_MATCH, "Str"));
  }

  /**
   * Test {@link SelectorUtils#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenEmptyString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.match(SelectorUtils.DEEP_TREE_MATCH, ""));
  }

  /**
   * Test {@link SelectorUtils#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenEmptyString_thenReturnTrue2() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.match("", ""));
  }

  /**
   * Test {@link SelectorUtils#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenPattern_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.match("Pattern", "Str"));
  }

  /**
   * Test {@link SelectorUtils#match(String, String)} with {@code pattern}, {@code str}.
   * <ul>
   *   <li>When {@code ?}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#match(String, String)}
   */
  @Test
  public void testMatchWithPatternStr_whenQuestionMark_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.match("?", "*"));
  }

  /**
   * Test {@link SelectorUtils#tokenizePath(String, String)} with {@code path}, {@code separator}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#tokenizePath(String, String)}
   */
  @Test
  public void testTokenizePathWithPathSeparator_whenEmptyString_thenReturnEmpty() {
    // Arrange and Act
    Vector<String> actualTokenizePathResult = SelectorUtils.tokenizePath("", "Separator");

    // Assert
    assertTrue(actualTokenizePathResult.isEmpty());
  }

  /**
   * Test {@link SelectorUtils#tokenizePath(String, String)} with {@code path}, {@code separator}.
   * <ul>
   *   <li>When {@code Path}.</li>
   *   <li>Then return size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#tokenizePath(String, String)}
   */
  @Test
  public void testTokenizePathWithPathSeparator_whenPath_thenReturnSizeIsTwo() {
    // Arrange and Act
    Vector<String> actualTokenizePathResult = SelectorUtils.tokenizePath("Path", "Separator");

    // Assert
    assertEquals(2, actualTokenizePathResult.size());
    assertEquals("P", actualTokenizePathResult.get(0));
    assertEquals("h", actualTokenizePathResult.get(1));
  }

  /**
   * Test {@link SelectorUtils#tokenizePath(String)} with {@code path}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#tokenizePath(String)}
   */
  @Test
  public void testTokenizePathWithPath_whenEmptyString_thenReturnEmpty() {
    // Arrange and Act
    Vector<String> actualTokenizePathResult = SelectorUtils.tokenizePath("");

    // Assert
    assertTrue(actualTokenizePathResult.isEmpty());
  }

  /**
   * Test {@link SelectorUtils#tokenizePath(String)} with {@code path}.
   * <ul>
   *   <li>When {@code Path}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#tokenizePath(String)}
   */
  @Test
  public void testTokenizePathWithPath_whenPath_thenReturnSizeIsOne() {
    // Arrange and Act
    Vector<String> actualTokenizePathResult = SelectorUtils.tokenizePath("Path");

    // Assert
    assertEquals(1, actualTokenizePathResult.size());
    assertEquals("Path", actualTokenizePathResult.get(0));
  }

  /**
   * Test {@link SelectorUtils#tokenizePathAsArray(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#tokenizePathAsArray(String)}
   */
  @Test
  public void testTokenizePathAsArray_whenEmptyString_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, SelectorUtils.tokenizePathAsArray("").length);
  }

  /**
   * Test {@link SelectorUtils#tokenizePathAsArray(String)}.
   * <ul>
   *   <li>When {@code Path}.</li>
   *   <li>Then return array of {@link String} with {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#tokenizePathAsArray(String)}
   */
  @Test
  public void testTokenizePathAsArray_whenPath_thenReturnArrayOfStringWithPath() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"Path"}, SelectorUtils.tokenizePathAsArray("Path"));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(File, File, int)} with {@code File}, {@code File}, {@code int}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(File, File, int)}
   */
  @Test
  public void testIsOutOfDateWithFileFileInt_thenReturnFalse() {
    // Arrange
    File src = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(
        SelectorUtils.isOutOfDate(src, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(File, File, int)} with {@code File}, {@code File}, {@code int}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(File, File, int)}
   */
  @Test
  public void testIsOutOfDateWithFileFileInt_thenReturnFalse2() {
    // Arrange
    File src = Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile();

    // Act and Assert
    assertFalse(
        SelectorUtils.isOutOfDate(src, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(File, File, int)} with {@code File}, {@code File}, {@code int}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(File, File, int)}
   */
  @Test
  public void testIsOutOfDateWithFileFileInt_thenReturnTrue() {
    // Arrange
    File src = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(
        SelectorUtils.isOutOfDate(src, Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile(), 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(File, File, int)} with {@code File}, {@code File}, {@code int}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is empty string toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(File, File, int)}
   */
  @Test
  public void testIsOutOfDateWithFileFileInt_whenPropertyIsJavaIoTmpdirIsEmptyStringToFile() {
    // Arrange
    File src = Paths.get(System.getProperty("java.io.tmpdir"), "").toFile();

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, int)} with {@code Resource}, {@code Resource}, {@code int}.
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, int)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceInt() {
    // Arrange
    Resource src = new Resource("Name", true, 1L);

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, new Resource(), 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, int)} with {@code Resource}, {@code Resource}, {@code int}.
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, int)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceInt2() {
    // Arrange
    FileResource src = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, new Resource(), 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, int)} with {@code Resource}, {@code Resource}, {@code int}.
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, int)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceInt3() {
    // Arrange
    Resource r = new Resource();
    MappedResource src = new MappedResource(r, new CutDirsMapper());

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, new Resource(), 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, int)} with {@code Resource}, {@code Resource}, {@code int}.
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, int)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceInt4() {
    // Arrange
    Resource src = new Resource("Name", true, 1L);

    // Act and Assert
    assertFalse(SelectorUtils.isOutOfDate(src, new Resource("Name", true, 1L), 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, int)} with {@code Resource}, {@code Resource}, {@code int}.
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, int)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceInt5() {
    // Arrange
    FileResource src = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, new Resource("Name", true, 1L), 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, int)} with {@code Resource}, {@code Resource}, {@code int}.
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, int)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceInt6() {
    // Arrange
    FileResource r = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    MappedResource src = new MappedResource(r, new CutDirsMapper());

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, new Resource(), 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, int)} with {@code Resource}, {@code Resource}, {@code int}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, int)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceInt_givenFileAttributeIsNull() {
    // Arrange
    FileResource src = new FileResource();
    src.setName("file attribute is null!");

    // Act and Assert
    assertFalse(SelectorUtils.isOutOfDate(src, new Resource(), 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, int)} with {@code Resource}, {@code Resource}, {@code int}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Resource#Resource()} Exists is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, int)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceInt_givenTrue_whenResourceExistsIsTrue() {
    // Arrange
    Resource src = new Resource();
    src.setExists(true);
    src.setLastModified(Resource.UNKNOWN_DATETIME);

    Resource target = new Resource();
    target.setExists(true);
    target.setLastModified(Resource.UNKNOWN_DATETIME);

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, target, 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, int)} with {@code Resource}, {@code Resource}, {@code int}.
   * <ul>
   *   <li>Given {@link Resource#UNKNOWN_SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, int)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceInt_givenUnknown_size() {
    // Arrange
    Resource src = new Resource();
    src.setExists(true);
    src.setLastModified(Resource.UNKNOWN_DATETIME);

    Resource target = new Resource();
    target.setExists(true);
    target.setLastModified(Resource.UNKNOWN_SIZE);

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, target, 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, int)} with {@code Resource}, {@code Resource}, {@code int}.
   * <ul>
   *   <li>When {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, int)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceInt_whenJavaConstantResource_thenReturnFalse() {
    // Arrange
    JavaConstantResource src = new JavaConstantResource();

    // Act and Assert
    assertFalse(SelectorUtils.isOutOfDate(src, new Resource(), 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, int)} with {@code Resource}, {@code Resource}, {@code int}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, int)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceInt_whenResourceWithName_thenReturnFalse() {
    // Arrange
    Resource src = new Resource("Name");

    // Act and Assert
    assertFalse(SelectorUtils.isOutOfDate(src, new Resource(), 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, int)} with {@code Resource}, {@code Resource}, {@code int}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, int)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceInt_whenResource_thenReturnTrue() {
    // Arrange
    Resource src = new Resource();

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, new Resource(), 1));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, long)} with {@code Resource}, {@code Resource}, {@code long}.
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, long)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceLong() {
    // Arrange
    Resource src = new Resource("Name", true, 1L);

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, new Resource(), 1L));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, long)} with {@code Resource}, {@code Resource}, {@code long}.
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, long)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceLong2() {
    // Arrange
    FileResource src = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, new Resource(), 1L));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, long)} with {@code Resource}, {@code Resource}, {@code long}.
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, long)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceLong3() {
    // Arrange
    Resource r = new Resource();
    MappedResource src = new MappedResource(r, new CutDirsMapper());

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, new Resource(), 1L));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, long)} with {@code Resource}, {@code Resource}, {@code long}.
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, long)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceLong4() {
    // Arrange
    Resource src = new Resource("Name", true, 1L);

    // Act and Assert
    assertFalse(SelectorUtils.isOutOfDate(src, new Resource("Name", true, 1L), 1L));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, long)} with {@code Resource}, {@code Resource}, {@code long}.
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, long)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceLong5() {
    // Arrange
    FileResource src = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, new Resource("Name", true, 1L), 1L));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, long)} with {@code Resource}, {@code Resource}, {@code long}.
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, long)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceLong6() {
    // Arrange
    FileResource r = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    MappedResource src = new MappedResource(r, new CutDirsMapper());

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, new Resource(), 1L));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, long)} with {@code Resource}, {@code Resource}, {@code long}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, long)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceLong_givenFileAttributeIsNull() {
    // Arrange
    FileResource src = new FileResource();
    src.setName("file attribute is null!");

    // Act and Assert
    assertFalse(SelectorUtils.isOutOfDate(src, new Resource(), 1L));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, long)} with {@code Resource}, {@code Resource}, {@code long}.
   * <ul>
   *   <li>Given {@link Resource#UNKNOWN_DATETIME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, long)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceLong_givenUnknown_datetime() {
    // Arrange
    Resource src = new Resource();
    src.setLastModified(Resource.UNKNOWN_DATETIME);
    src.setExists(true);

    Resource target = new Resource();
    target.setLastModified(Resource.UNKNOWN_DATETIME);
    target.setExists(true);

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, target, 1L));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, long)} with {@code Resource}, {@code Resource}, {@code long}.
   * <ul>
   *   <li>Given {@link Resource#UNKNOWN_SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, long)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceLong_givenUnknown_size() {
    // Arrange
    Resource src = new Resource();
    src.setLastModified(Resource.UNKNOWN_DATETIME);
    src.setExists(true);

    Resource target = new Resource();
    target.setLastModified(Resource.UNKNOWN_SIZE);
    target.setExists(true);

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, target, 1L));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, long)} with {@code Resource}, {@code Resource}, {@code long}.
   * <ul>
   *   <li>When {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, long)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceLong_whenJavaConstantResource_thenReturnFalse() {
    // Arrange
    JavaConstantResource src = new JavaConstantResource();

    // Act and Assert
    assertFalse(SelectorUtils.isOutOfDate(src, new Resource(), 1L));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, long)} with {@code Resource}, {@code Resource}, {@code long}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, long)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceLong_whenResourceWithName_thenReturnFalse() {
    // Arrange
    Resource src = new Resource("Name");

    // Act and Assert
    assertFalse(SelectorUtils.isOutOfDate(src, new Resource(), 1L));
  }

  /**
   * Test {@link SelectorUtils#isOutOfDate(Resource, Resource, long)} with {@code Resource}, {@code Resource}, {@code long}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#isOutOfDate(Resource, Resource, long)}
   */
  @Test
  public void testIsOutOfDateWithResourceResourceLong_whenResource_thenReturnTrue() {
    // Arrange
    Resource src = new Resource();

    // Act and Assert
    assertTrue(SelectorUtils.isOutOfDate(src, new Resource(), 1L));
  }

  /**
   * Test {@link SelectorUtils#removeWhitespace(String)}.
   * <ul>
   *   <li>When {@code Input}.</li>
   *   <li>Then return {@code Input}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#removeWhitespace(String)}
   */
  @Test
  public void testRemoveWhitespace_whenInput_thenReturnInput() {
    // Arrange, Act and Assert
    assertEquals("Input", SelectorUtils.removeWhitespace("Input"));
  }

  /**
   * Test {@link SelectorUtils#removeWhitespace(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#removeWhitespace(String)}
   */
  @Test
  public void testRemoveWhitespace_whenNull_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", SelectorUtils.removeWhitespace(null));
  }

  /**
   * Test {@link SelectorUtils#hasWildcards(String)}.
   * <ul>
   *   <li>When {@code *}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#hasWildcards(String)}
   */
  @Test
  public void testHasWildcards_whenAsterisk_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.hasWildcards("*"));
  }

  /**
   * Test {@link SelectorUtils#hasWildcards(String)}.
   * <ul>
   *   <li>When {@code Input}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#hasWildcards(String)}
   */
  @Test
  public void testHasWildcards_whenInput_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(SelectorUtils.hasWildcards("Input"));
  }

  /**
   * Test {@link SelectorUtils#hasWildcards(String)}.
   * <ul>
   *   <li>When {@code ?}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#hasWildcards(String)}
   */
  @Test
  public void testHasWildcards_whenQuestionMark_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(SelectorUtils.hasWildcards("?"));
  }

  /**
   * Test {@link SelectorUtils#rtrimWildcardTokens(String)}.
   * <ul>
   *   <li>When {@code *}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#rtrimWildcardTokens(String)}
   */
  @Test
  public void testRtrimWildcardTokens_whenAsterisk_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", SelectorUtils.rtrimWildcardTokens("*"));
  }

  /**
   * Test {@link SelectorUtils#rtrimWildcardTokens(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#rtrimWildcardTokens(String)}
   */
  @Test
  public void testRtrimWildcardTokens_whenEmptyString_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", SelectorUtils.rtrimWildcardTokens(""));
  }

  /**
   * Test {@link SelectorUtils#rtrimWildcardTokens(String)}.
   * <ul>
   *   <li>When {@code Input}.</li>
   *   <li>Then return {@code Input}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#rtrimWildcardTokens(String)}
   */
  @Test
  public void testRtrimWildcardTokens_whenInput_thenReturnInput() {
    // Arrange, Act and Assert
    assertEquals("Input", SelectorUtils.rtrimWildcardTokens("Input"));
  }

  /**
   * Test {@link SelectorUtils#rtrimWildcardTokens(String)}.
   * <ul>
   *   <li>When {@code ?}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectorUtils#rtrimWildcardTokens(String)}
   */
  @Test
  public void testRtrimWildcardTokens_whenQuestionMark_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", SelectorUtils.rtrimWildcardTokens("?"));
  }
}
