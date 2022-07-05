package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class GlobPatternMapperDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>default or parameterless constructor of {@link GlobPatternMapper}
  *   <li>{@link GlobPatternMapper#setCaseSensitive(boolean)}
  *   <li>{@link GlobPatternMapper#setHandleDirSep(boolean)}
  *   <li>{@link GlobPatternMapper#getHandleDirSep()}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    GlobPatternMapper actualGlobPatternMapper = new GlobPatternMapper();
    actualGlobPatternMapper.setCaseSensitive(true);
    actualGlobPatternMapper.setHandleDirSep(true);

    // Assert
    assertTrue(actualGlobPatternMapper.getHandleDirSep());
  }

  /**
   * Method under test: {@link GlobPatternMapper#extractVariablePart(String)}
   */
  @Test
  public void testExtractVariablePart() {
    // Arrange, Act and Assert
    assertEquals("Name", (new GlobPatternMapper()).extractVariablePart("Name"));
    assertEquals("Name", (new PackageNameMapper()).extractVariablePart("Name"));
  }

  /**
   * Method under test: {@link GlobPatternMapper#extractVariablePart(String)}
   */
  @Test
  public void testExtractVariablePart2() {
    // Arrange
    PackageNameMapper packageNameMapper = new PackageNameMapper();
    packageNameMapper.setHandleDirSep(true);

    // Act and Assert
    assertEquals("Name", packageNameMapper.extractVariablePart("Name"));
  }

  /**
   * Method under test: {@link GlobPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName() {
    // Arrange, Act and Assert
    assertNull((new GlobPatternMapper()).mapFileName("foo.txt"));
    assertNull((new GlobPatternMapper()).mapFileName(null));
  }

  /**
   * Method under test: {@link GlobPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName2() {
    // Arrange
    GlobPatternMapper globPatternMapper = new GlobPatternMapper();
    globPatternMapper.setHandleDirSep(true);

    // Act and Assert
    assertNull(globPatternMapper.mapFileName("foo.txt"));
  }

  /**
   * Method under test: {@link GlobPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName3() {
    // Arrange
    GlobPatternMapper globPatternMapper = new GlobPatternMapper();
    globPatternMapper.setHandleDirSep(true);

    // Act and Assert
    assertNull(globPatternMapper.mapFileName("\\"));
  }

  /**
   * Method under test: {@link GlobPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName4() {
    // Arrange
    GlobPatternMapper globPatternMapper = new GlobPatternMapper();
    globPatternMapper.setCaseSensitive(false);

    // Act and Assert
    assertNull(globPatternMapper.mapFileName("foo.txt"));
  }

  /**
   * Method under test: {@link GlobPatternMapper#setFrom(String)}
   */
  @Test
  public void testSetFrom() {
    // Arrange
    GlobPatternMapper globPatternMapper = new GlobPatternMapper();

    // Act
    globPatternMapper.setFrom("jane.doe@example.org");

    // Assert
    assertEquals(20, globPatternMapper.prefixLength);
    assertEquals(0, globPatternMapper.postfixLength);
    assertEquals("jane.doe@example.org", globPatternMapper.fromPrefix);
    assertEquals("", globPatternMapper.fromPostfix);
  }

  /**
   * Method under test: {@link GlobPatternMapper#setFrom(String)}
   */
  @Test
  public void testSetFrom2() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new GlobPatternMapper()).setFrom(null));
  }

  /**
   * Method under test: {@link GlobPatternMapper#setTo(String)}
   */
  @Test
  public void testSetTo() {
    // Arrange
    GlobPatternMapper globPatternMapper = new GlobPatternMapper();

    // Act
    globPatternMapper.setTo("alice.liddell@example.org");

    // Assert
    assertEquals("alice.liddell@example.org", globPatternMapper.toPrefix);
    assertEquals("", globPatternMapper.toPostfix);
  }

  /**
   * Method under test: {@link GlobPatternMapper#setTo(String)}
   */
  @Test
  public void testSetTo2() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new GlobPatternMapper()).setTo(null));
  }
}

