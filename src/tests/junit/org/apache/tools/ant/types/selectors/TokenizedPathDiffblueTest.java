package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import java.io.File;
import java.nio.file.Paths;
import org.junit.Test;

public class TokenizedPathDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TokenizedPath#TokenizedPath(String, String[])}
   *   <li>{@link TokenizedPath#getTokens()}
   *   <li>{@link TokenizedPath#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    String[] tokens = new String[]{"ABC123"};

    // Act
    TokenizedPath actualTokenizedPath = new TokenizedPath("Path", tokens);
    String[] actualTokens = actualTokenizedPath.getTokens();

    // Assert
    assertEquals("Path", actualTokenizedPath.toString());
    assertSame(tokens, actualTokens);
    assertArrayEquals(new String[]{"ABC123"}, actualTokens);
  }

  /**
   * Test {@link TokenizedPath#TokenizedPath(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return toString is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#TokenizedPath(String)}
   */
  @Test
  public void testNewTokenizedPath_whenEmptyString_thenReturnToStringIsEmptyString() {
    // Arrange and Act
    TokenizedPath actualTokenizedPath = new TokenizedPath("");

    // Assert
    assertEquals("", actualTokenizedPath.toString());
    TokenizedPattern toPatternResult = actualTokenizedPath.toPattern();
    assertEquals("", toPatternResult.getPattern());
    assertEquals("", toPatternResult.toString());
    assertEquals(0, actualTokenizedPath.depth());
    assertEquals(0, toPatternResult.depth());
    assertEquals(0, actualTokenizedPath.getTokens().length);
  }

  /**
   * Test {@link TokenizedPath#TokenizedPath(TokenizedPath, String)}.
   * <ul>
   *   <li>When {@link TokenizedPath#EMPTY_PATH}.</li>
   *   <li>Then return toString is {@code Child}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#TokenizedPath(TokenizedPath, String)}
   */
  @Test
  public void testNewTokenizedPath_whenEmpty_path_thenReturnToStringIsChild() {
    // Arrange and Act
    TokenizedPath actualTokenizedPath = new TokenizedPath(TokenizedPath.EMPTY_PATH, "Child");

    // Assert
    assertEquals("Child", actualTokenizedPath.toString());
    TokenizedPattern toPatternResult = actualTokenizedPath.toPattern();
    assertEquals("Child", toPatternResult.getPattern());
    assertEquals("Child", toPatternResult.toString());
    assertEquals(1, actualTokenizedPath.depth());
    assertEquals(1, toPatternResult.depth());
    assertArrayEquals(new String[]{"Child"}, actualTokenizedPath.getTokens());
  }

  /**
   * Test {@link TokenizedPath#TokenizedPath(String)}.
   * <ul>
   *   <li>When {@code Path}.</li>
   *   <li>Then return toString is {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#TokenizedPath(String)}
   */
  @Test
  public void testNewTokenizedPath_whenPath_thenReturnToStringIsPath() {
    // Arrange and Act
    TokenizedPath actualTokenizedPath = new TokenizedPath("Path");

    // Assert
    assertEquals("Path", actualTokenizedPath.toString());
    TokenizedPattern toPatternResult = actualTokenizedPath.toPattern();
    assertEquals("Path", toPatternResult.getPattern());
    assertEquals("Path", toPatternResult.toString());
    assertEquals(1, actualTokenizedPath.depth());
    assertEquals(1, toPatternResult.depth());
    assertArrayEquals(new String[]{"Path"}, actualTokenizedPath.getTokens());
  }

  /**
   * Test {@link TokenizedPath#TokenizedPath(TokenizedPath, String)}.
   * <ul>
   *   <li>When {@link TokenizedPath#TokenizedPath(String)} with {@code Path}.</li>
   *   <li>Then return toString is {@code Path/Child}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#TokenizedPath(TokenizedPath, String)}
   */
  @Test
  public void testNewTokenizedPath_whenTokenizedPathWithPath_thenReturnToStringIsPathChild() {
    // Arrange and Act
    TokenizedPath actualTokenizedPath = new TokenizedPath(new TokenizedPath("Path"), "Child");

    // Assert
    assertEquals("Path/Child", actualTokenizedPath.toString());
    TokenizedPattern toPatternResult = actualTokenizedPath.toPattern();
    assertEquals("Path/Child", toPatternResult.getPattern());
    assertEquals("Path/Child", toPatternResult.toString());
    assertEquals(2, actualTokenizedPath.depth());
    assertEquals(2, toPatternResult.depth());
    assertArrayEquals(new String[]{"Path", "Child"}, actualTokenizedPath.getTokens());
  }

  /**
   * Test {@link TokenizedPath#depth()}.
   * <p>
   * Method under test: {@link TokenizedPath#depth()}
   */
  @Test
  public void testDepth() {
    // Arrange, Act and Assert
    assertEquals(0, TokenizedPath.EMPTY_PATH.depth());
  }

  /**
   * Test {@link TokenizedPath#findFile(File, boolean)} with {@code base}, {@code cs}.
   * <ul>
   *   <li>Given {@link TokenizedPath#TokenizedPath(String)} with path is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#findFile(File, boolean)}
   */
  @Test
  public void testFindFileWithBaseCs_givenTokenizedPathWithPathIsEmptyString() {
    // Arrange
    TokenizedPath tokenizedPath = new TokenizedPath("");

    // Act and Assert
    assertNull(tokenizedPath.findFile(Paths.get(System.getProperty("java.io.tmpdir"), "Path").toFile(), false));
  }

  /**
   * Test {@link TokenizedPath#findFile(File, boolean)} with {@code base}, {@code cs}.
   * <ul>
   *   <li>Then return Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#findFile(File, boolean)}
   */
  @Test
  public void testFindFileWithBaseCs_thenReturnPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    File base = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertSame(base, TokenizedPath.EMPTY_PATH.findFile(base, true));
  }

  /**
   * Test {@link TokenizedPath#findFile(File, boolean)} with {@code base}, {@code cs}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code foo} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#findFile(File, boolean)}
   */
  @Test
  public void testFindFileWithBaseCs_whenPropertyIsJavaIoTmpdirIsFooToFile() {
    // Arrange
    TokenizedPath tokenizedPath = new TokenizedPath("Path");

    // Act and Assert
    assertNull(tokenizedPath.findFile(Paths.get(System.getProperty("java.io.tmpdir"), "foo").toFile(), true));
  }

  /**
   * Test {@link TokenizedPath#findFile(File, boolean)} with {@code base}, {@code cs}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code foo} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#findFile(File, boolean)}
   */
  @Test
  public void testFindFileWithBaseCs_whenPropertyIsJavaIoTmpdirIsFooToFile2() {
    // Arrange
    TokenizedPath tokenizedPath = new TokenizedPath("Path");

    // Act and Assert
    assertNull(tokenizedPath.findFile(Paths.get(System.getProperty("java.io.tmpdir"), "foo").toFile(), false));
  }

  /**
   * Test {@link TokenizedPath#findFile(File, boolean)} with {@code base}, {@code cs}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code Path} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#findFile(File, boolean)}
   */
  @Test
  public void testFindFileWithBaseCs_whenPropertyIsJavaIoTmpdirIsPathToFile() {
    // Arrange
    TokenizedPath tokenizedPath = new TokenizedPath("Path");

    // Act and Assert
    assertNull(tokenizedPath.findFile(Paths.get(System.getProperty("java.io.tmpdir"), "Path").toFile(), false));
  }

  /**
   * Test {@link TokenizedPath#findFile(File, boolean)} with {@code base}, {@code cs}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#findFile(File, boolean)}
   */
  @Test
  public void testFindFileWithBaseCs_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    TokenizedPath tokenizedPath = new TokenizedPath("Path");

    // Act and Assert
    assertNull(tokenizedPath.findFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true));
  }

  /**
   * Test {@link TokenizedPath#findFile(File, boolean)} with {@code base}, {@code cs}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#findFile(File, boolean)}
   */
  @Test
  public void testFindFileWithBaseCs_whenPropertyIsJavaIoTmpdirIsTestTxtToFile2() {
    // Arrange
    TokenizedPath tokenizedPath = new TokenizedPath("Path");

    // Act and Assert
    assertNull(tokenizedPath.findFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), false));
  }

  /**
   * Test {@link TokenizedPath#toPattern()}.
   * <p>
   * Method under test: {@link TokenizedPath#toPattern()}
   */
  @Test
  public void testToPattern() {
    // Arrange and Act
    TokenizedPattern actualToPatternResult = TokenizedPath.EMPTY_PATH.toPattern();

    // Assert
    assertEquals(actualToPatternResult.EMPTY_PATTERN, actualToPatternResult);
  }

  /**
   * Test {@link TokenizedPath#isSymlink(File)}.
   * <ul>
   *   <li>Given {@link TokenizedPath#EMPTY_PATH}.</li>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#isSymlink(File)}
   */
  @Test
  public void testIsSymlink_givenEmpty_path_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange, Act and Assert
    assertFalse(
        TokenizedPath.EMPTY_PATH.isSymlink(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link TokenizedPath#isSymlink(File)}.
   * <ul>
   *   <li>Given {@link TokenizedPath#TokenizedPath(String)} with {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#isSymlink(File)}
   */
  @Test
  public void testIsSymlink_givenTokenizedPathWithPath() {
    // Arrange
    TokenizedPath tokenizedPath = new TokenizedPath("Path");

    // Act and Assert
    assertFalse(tokenizedPath.isSymlink(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link TokenizedPath#isSymlink(File)}.
   * <ul>
   *   <li>Given {@link TokenizedPath#TokenizedPath(String)} with {@code Path}.</li>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#isSymlink(File)}
   */
  @Test
  public void testIsSymlink_givenTokenizedPathWithPath_whenNull() {
    // Arrange, Act and Assert
    assertFalse((new TokenizedPath("Path")).isSymlink(null));
  }

  /**
   * Test {@link TokenizedPath#equals(Object)}, and {@link TokenizedPath#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TokenizedPath#equals(Object)}
   *   <li>{@link TokenizedPath#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    TokenizedPath tokenizedPath = TokenizedPath.EMPTY_PATH;
    TokenizedPath tokenizedPath2 = TokenizedPath.EMPTY_PATH;

    // Act and Assert
    assertEquals(tokenizedPath, tokenizedPath2);
    int expectedHashCodeResult = tokenizedPath.hashCode();
    assertEquals(expectedHashCodeResult, tokenizedPath2.hashCode());
  }

  /**
   * Test {@link TokenizedPath#equals(Object)}, and {@link TokenizedPath#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TokenizedPath#equals(Object)}
   *   <li>{@link TokenizedPath#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    TokenizedPath tokenizedPath = TokenizedPath.EMPTY_PATH;

    // Act and Assert
    assertEquals(tokenizedPath, tokenizedPath);
    int expectedHashCodeResult = tokenizedPath.hashCode();
    assertEquals(expectedHashCodeResult, tokenizedPath.hashCode());
  }

  /**
   * Test {@link TokenizedPath#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new TokenizedPath("Path"), TokenizedPath.EMPTY_PATH);
  }

  /**
   * Test {@link TokenizedPath#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(TokenizedPath.EMPTY_PATH, null);
  }

  /**
   * Test {@link TokenizedPath#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPath#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(TokenizedPath.EMPTY_PATH, "Different type to TokenizedPath");
  }
}
