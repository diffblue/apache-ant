package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class GlobPatternMapperDiffblueTest {
  /**
   * Test {@link GlobPatternMapper#setFrom(String)}.
   * <ul>
   *   <li>When {@code jane.doe@example.org}.</li>
   *   <li>Then {@link GlobPatternMapper} (default constructor) {@link GlobPatternMapper#fromPostfix} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link GlobPatternMapper#setFrom(String)}
   */
  @Test
  public void testSetFrom_whenJaneDoeExampleOrg_thenGlobPatternMapperFromPostfixIsEmptyString() {
    // Arrange
    GlobPatternMapper globPatternMapper = new GlobPatternMapper();

    // Act
    globPatternMapper.setFrom("jane.doe@example.org");

    // Assert
    assertEquals("", globPatternMapper.fromPostfix);
    assertEquals("jane.doe@example.org", globPatternMapper.fromPrefix);
    assertEquals(20, globPatternMapper.prefixLength);
  }

  /**
   * Test {@link GlobPatternMapper#setFrom(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GlobPatternMapper#setFrom(String)}
   */
  @Test
  public void testSetFrom_whenNull_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new GlobPatternMapper()).setFrom(null));
  }

  /**
   * Test {@link GlobPatternMapper#setTo(String)}.
   * <ul>
   *   <li>When {@code alice.liddell@example.org}.</li>
   *   <li>Then {@link GlobPatternMapper} (default constructor) {@link GlobPatternMapper#toPostfix} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link GlobPatternMapper#setTo(String)}
   */
  @Test
  public void testSetTo_whenAliceLiddellExampleOrg_thenGlobPatternMapperToPostfixIsEmptyString() {
    // Arrange
    GlobPatternMapper globPatternMapper = new GlobPatternMapper();

    // Act
    globPatternMapper.setTo("alice.liddell@example.org");

    // Assert
    assertEquals("", globPatternMapper.toPostfix);
    assertEquals("alice.liddell@example.org", globPatternMapper.toPrefix);
  }

  /**
   * Test {@link GlobPatternMapper#setTo(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GlobPatternMapper#setTo(String)}
   */
  @Test
  public void testSetTo_whenNull_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new GlobPatternMapper()).setTo(null));
  }

  /**
   * Test {@link GlobPatternMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link GlobPatternMapper} (default constructor) CaseSensitive is {@code false}.</li>
   *   <li>When {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GlobPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenGlobPatternMapperCaseSensitiveIsFalse_whenFooTxt() {
    // Arrange
    GlobPatternMapper globPatternMapper = new GlobPatternMapper();
    globPatternMapper.setCaseSensitive(false);

    // Act and Assert
    assertNull(globPatternMapper.mapFileName("foo.txt"));
  }

  /**
   * Test {@link GlobPatternMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link GlobPatternMapper} (default constructor) HandleDirSep is {@code true}.</li>
   *   <li>When {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GlobPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenGlobPatternMapperHandleDirSepIsTrue_whenBackslash() {
    // Arrange
    GlobPatternMapper globPatternMapper = new GlobPatternMapper();
    globPatternMapper.setHandleDirSep(true);

    // Act and Assert
    assertNull(globPatternMapper.mapFileName("\\"));
  }

  /**
   * Test {@link GlobPatternMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link GlobPatternMapper} (default constructor) HandleDirSep is {@code true}.</li>
   *   <li>When {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GlobPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenGlobPatternMapperHandleDirSepIsTrue_whenFooTxt() {
    // Arrange
    GlobPatternMapper globPatternMapper = new GlobPatternMapper();
    globPatternMapper.setHandleDirSep(true);

    // Act and Assert
    assertNull(globPatternMapper.mapFileName("foo.txt"));
  }

  /**
   * Test {@link GlobPatternMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link GlobPatternMapper} (default constructor).</li>
   *   <li>When {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GlobPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenGlobPatternMapper_whenFooTxt() {
    // Arrange, Act and Assert
    assertNull((new GlobPatternMapper()).mapFileName("foo.txt"));
  }

  /**
   * Test {@link GlobPatternMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link GlobPatternMapper} (default constructor).</li>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GlobPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenGlobPatternMapper_whenNull() {
    // Arrange, Act and Assert
    assertNull((new GlobPatternMapper()).mapFileName(null));
  }

  /**
   * Test {@link GlobPatternMapper#extractVariablePart(String)}.
   * <ul>
   *   <li>Given {@link GlobPatternMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link GlobPatternMapper#extractVariablePart(String)}
   */
  @Test
  public void testExtractVariablePart_givenGlobPatternMapper() {
    // Arrange, Act and Assert
    assertEquals("Name", (new GlobPatternMapper()).extractVariablePart("Name"));
  }

  /**
   * Test {@link GlobPatternMapper#extractVariablePart(String)}.
   * <ul>
   *   <li>Given {@link PackageNameMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link GlobPatternMapper#extractVariablePart(String)}
   */
  @Test
  public void testExtractVariablePart_givenPackageNameMapper() {
    // Arrange, Act and Assert
    assertEquals("Name", (new PackageNameMapper()).extractVariablePart("Name"));
  }

  /**
   * Test {@link GlobPatternMapper#extractVariablePart(String)}.
   * <ul>
   *   <li>Given {@link PackageNameMapper} (default constructor) HandleDirSep is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GlobPatternMapper#extractVariablePart(String)}
   */
  @Test
  public void testExtractVariablePart_givenPackageNameMapperHandleDirSepIsTrue() {
    // Arrange
    PackageNameMapper packageNameMapper = new PackageNameMapper();
    packageNameMapper.setHandleDirSep(true);

    // Act and Assert
    assertEquals("Name", packageNameMapper.extractVariablePart("Name"));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link GlobPatternMapper}
   *   <li>{@link GlobPatternMapper#setCaseSensitive(boolean)}
   *   <li>{@link GlobPatternMapper#setHandleDirSep(boolean)}
   *   <li>{@link GlobPatternMapper#getHandleDirSep()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    GlobPatternMapper actualGlobPatternMapper = new GlobPatternMapper();
    actualGlobPatternMapper.setCaseSensitive(true);
    actualGlobPatternMapper.setHandleDirSep(true);

    // Assert
    assertTrue(actualGlobPatternMapper.getHandleDirSep());
  }
}
