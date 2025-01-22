package org.apache.tools.ant.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.util.regexp.Jdk14RegexpMatcher;
import org.apache.tools.ant.util.regexp.RegexpMatcher;
import org.junit.Test;

public class RegexpPatternMapperDiffblueTest {
  /**
   * Test {@link RegexpPatternMapper#RegexpPatternMapper()}.
   * <p>
   * Method under test: default or parameterless constructor of {@link RegexpPatternMapper}
   */
  @Test
  public void testNewRegexpPatternMapper() throws BuildException {
    // Arrange and Act
    RegexpPatternMapper actualRegexpPatternMapper = new RegexpPatternMapper();

    // Assert
    RegexpMatcher regexpMatcher = actualRegexpPatternMapper.reg;
    assertTrue(regexpMatcher instanceof Jdk14RegexpMatcher);
    assertEquals("", actualRegexpPatternMapper.result.toString());
    assertNull(actualRegexpPatternMapper.to);
    assertNull(regexpMatcher.getPattern());
  }

  /**
   * Test {@link RegexpPatternMapper#setFrom(String)}.
   * <ul>
   *   <li>When {@code jane.doe@example.org}.</li>
   *   <li>Then {@link RegexpPatternMapper#RegexpPatternMapper()} {@link RegexpPatternMapper#reg} {@link Jdk14RegexpMatcher}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpPatternMapper#setFrom(String)}
   */
  @Test
  public void testSetFrom_whenJaneDoeExampleOrg_thenRegexpPatternMapperRegJdk14RegexpMatcher() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();

    // Act
    regexpPatternMapper.setFrom("jane.doe@example.org");

    // Assert
    RegexpMatcher regexpMatcher = regexpPatternMapper.reg;
    assertTrue(regexpMatcher instanceof Jdk14RegexpMatcher);
    assertEquals("jane.doe@example.org", regexpMatcher.getPattern());
  }

  /**
   * Test {@link RegexpPatternMapper#setFrom(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpPatternMapper#setFrom(String)}
   */
  @Test
  public void testSetFrom_whenNull_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new RegexpPatternMapper()).setFrom(null));
  }

  /**
   * Test {@link RegexpPatternMapper#setTo(String)}.
   * <ul>
   *   <li>Then {@link RegexpPatternMapper#RegexpPatternMapper()} {@link RegexpPatternMapper#to} is {@code alice.liddell@example.org} toCharArray.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpPatternMapper#setTo(String)}
   */
  @Test
  public void testSetTo_thenRegexpPatternMapperToIsAliceLiddellExampleOrgToCharArray() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();

    // Act
    regexpPatternMapper.setTo("alice.liddell@example.org");

    // Assert
    assertArrayEquals("alice.liddell@example.org".toCharArray(), regexpPatternMapper.to);
  }

  /**
   * Test {@link RegexpPatternMapper#setTo(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpPatternMapper#setTo(String)}
   */
  @Test
  public void testSetTo_whenNull_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new RegexpPatternMapper()).setTo(null));
  }

  /**
   * Test {@link RegexpPatternMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link RegexpPatternMapper#RegexpPatternMapper()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenRegexpPatternMapper() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();

    // Act
    String[] actualMapFileNameResult = regexpPatternMapper.mapFileName("foo.txt");

    // Assert
    assertEquals("", regexpPatternMapper.result.toString());
    assertNull(actualMapFileNameResult);
  }

  /**
   * Test {@link RegexpPatternMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link RegexpPatternMapper#RegexpPatternMapper()} From is {@code foo}.</li>
   *   <li>When {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenRegexpPatternMapperFromIsFoo_whenBackslash() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setHandleDirSep(false);
    regexpPatternMapper.setFrom("foo");
    regexpPatternMapper.setTo("foo");

    // Act
    String[] actualMapFileNameResult = regexpPatternMapper.mapFileName("\\");

    // Assert
    assertEquals("", regexpPatternMapper.result.toString());
    assertNull(actualMapFileNameResult);
  }

  /**
   * Test {@link RegexpPatternMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link RegexpPatternMapper#RegexpPatternMapper()} From is {@code foo}.</li>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenRegexpPatternMapperFromIsFoo_whenNull() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setHandleDirSep(false);
    regexpPatternMapper.setFrom("foo");
    regexpPatternMapper.setTo("foo");

    // Act
    String[] actualMapFileNameResult = regexpPatternMapper.mapFileName(null);

    // Assert
    assertEquals("", regexpPatternMapper.result.toString());
    assertNull(actualMapFileNameResult);
  }

  /**
   * Test {@link RegexpPatternMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link RegexpPatternMapper#RegexpPatternMapper()} HandleDirSep is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenRegexpPatternMapperHandleDirSepIsTrue() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setHandleDirSep(true);

    // Act
    String[] actualMapFileNameResult = regexpPatternMapper.mapFileName("foo.txt");

    // Assert
    assertEquals("", regexpPatternMapper.result.toString());
    assertNull(actualMapFileNameResult);
  }

  /**
   * Test {@link RegexpPatternMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link RegexpPatternMapper#RegexpPatternMapper()} HandleDirSep is {@code true}.</li>
   *   <li>When {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenRegexpPatternMapperHandleDirSepIsTrue_whenBackslash() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setHandleDirSep(true);

    // Act
    String[] actualMapFileNameResult = regexpPatternMapper.mapFileName("\\");

    // Assert
    assertEquals("", regexpPatternMapper.result.toString());
    assertNull(actualMapFileNameResult);
  }

  /**
   * Test {@link RegexpPatternMapper#mapFileName(String)}.
   * <ul>
   *   <li>Then {@link RegexpPatternMapper#RegexpPatternMapper()} {@link RegexpPatternMapper#result} toString is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_thenRegexpPatternMapperResultToStringIsBackslash() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setHandleDirSep(false);
    regexpPatternMapper.setFrom("foo");
    regexpPatternMapper.setTo("\\");

    // Act
    String[] actualMapFileNameResult = regexpPatternMapper.mapFileName("foo.txt");

    // Assert
    assertEquals("\\", regexpPatternMapper.result.toString());
    assertArrayEquals(new String[]{"\\"}, actualMapFileNameResult);
  }

  /**
   * Test {@link RegexpPatternMapper#mapFileName(String)}.
   * <ul>
   *   <li>Then {@link RegexpPatternMapper#RegexpPatternMapper()} {@link RegexpPatternMapper#result} toString is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_thenRegexpPatternMapperResultToStringIsFoo() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setHandleDirSep(false);
    regexpPatternMapper.setFrom("foo");
    regexpPatternMapper.setTo("foo");

    // Act
    String[] actualMapFileNameResult = regexpPatternMapper.mapFileName("foo.txt");

    // Assert
    assertEquals("foo", regexpPatternMapper.result.toString());
    assertArrayEquals(new String[]{"foo"}, actualMapFileNameResult);
  }

  /**
   * Test {@link RegexpPatternMapper#replaceReferences(String)}.
   * <p>
   * Method under test: {@link RegexpPatternMapper#replaceReferences(String)}
   */
  @Test
  public void testReplaceReferences() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setTo("alice.liddell@example.org");
    regexpPatternMapper.setFrom("jane.doe@example.org");

    // Act
    String actualReplaceReferencesResult = regexpPatternMapper.replaceReferences("Source");

    // Assert
    assertEquals("alice.liddell@example.org", regexpPatternMapper.result.toString());
    assertEquals("alice.liddell@example.org", actualReplaceReferencesResult);
  }

  /**
   * Test {@link RegexpPatternMapper#replaceReferences(String)}.
   * <ul>
   *   <li>Then {@link RegexpPatternMapper#RegexpPatternMapper()} {@link RegexpPatternMapper#result} toString is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpPatternMapper#replaceReferences(String)}
   */
  @Test
  public void testReplaceReferences_thenRegexpPatternMapperResultToStringIsBackslash() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setTo("\\");
    regexpPatternMapper.setFrom("jane.doe@example.org");

    // Act
    String actualReplaceReferencesResult = regexpPatternMapper.replaceReferences("Source");

    // Assert
    assertEquals("\\", regexpPatternMapper.result.toString());
    assertEquals("\\", actualReplaceReferencesResult);
  }
}
