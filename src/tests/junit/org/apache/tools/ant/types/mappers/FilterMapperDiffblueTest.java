package org.apache.tools.ant.types.mappers;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.UnsupportedAttributeException;
import org.apache.tools.ant.filters.ExpandProperties;
import org.apache.tools.ant.types.AntFilterReader;
import org.junit.Test;

public class FilterMapperDiffblueTest {
  /**
   * Test {@link FilterMapper#setFrom(String)}.
   * <p>
   * Method under test: {@link FilterMapper#setFrom(String)}
   */
  @Test
  public void testSetFrom() {
    // Arrange, Act and Assert
    assertThrows(UnsupportedAttributeException.class, () -> (new FilterMapper()).setFrom("jane.doe@example.org"));
  }

  /**
   * Test {@link FilterMapper#setTo(String)}.
   * <p>
   * Method under test: {@link FilterMapper#setTo(String)}
   */
  @Test
  public void testSetTo() {
    // Arrange, Act and Assert
    assertThrows(UnsupportedAttributeException.class, () -> (new FilterMapper()).setTo("alice.liddell@example.org"));
  }

  /**
   * Test {@link FilterMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) addExpandProperties {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilterMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenFilterMapperAddExpandPropertiesExpandProperties() {
    // Arrange
    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addExpandProperties(new ExpandProperties());
    filterMapper.addFilterReader(new AntFilterReader());

    // Act and Assert
    assertArrayEquals(new String[]{"foo.txt"}, filterMapper.mapFileName("foo.txt"));
  }

  /**
   * Test {@link FilterMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link FilterMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenFilterMapperProjectIsProject() {
    // Arrange
    FilterMapper filterMapper = new FilterMapper();
    filterMapper.setProject(new Project());
    filterMapper.addExpandProperties(new ExpandProperties());
    filterMapper.addFilterReader(new AntFilterReader());

    // Act and Assert
    assertArrayEquals(new String[]{"foo.txt"}, filterMapper.mapFileName("foo.txt"));
  }

  /**
   * Test {@link FilterMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor).</li>
   *   <li>When empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilterMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenFilterMapper_whenEmptyString_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new FilterMapper()).mapFileName(""));
  }

  /**
   * Test {@link FilterMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor).</li>
   *   <li>When {@code foo.txt}.</li>
   *   <li>Then return array of {@link String} with {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilterMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenFilterMapper_whenFooTxt_thenReturnArrayOfStringWithFooTxt() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"foo.txt"}, (new FilterMapper()).mapFileName("foo.txt"));
  }

  /**
   * Test {@link FilterMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilterMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenFilterMapper_whenNull_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new FilterMapper()).mapFileName(null));
  }

  /**
   * Test {@link FilterMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilterMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.setProject(project);
    filterMapper.addExpandProperties(new ExpandProperties());
    filterMapper.addFilterReader(new AntFilterReader());

    // Act and Assert
    assertArrayEquals(new String[]{"foo.txt"}, filterMapper.mapFileName("foo.txt"));
  }

  /**
   * Test {@link FilterMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilterMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    FilterMapper filterMapper = new FilterMapper();
    filterMapper.setProject(project);
    filterMapper.addExpandProperties(new ExpandProperties());
    filterMapper.addFilterReader(new AntFilterReader());

    // Act and Assert
    assertArrayEquals(new String[]{"foo.txt"}, filterMapper.mapFileName("foo.txt"));
  }

  /**
   * Test {@link FilterMapper#mapFileName(String)}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilterMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_thenReturnArrayOfStringWithFooTxt() {
    // Arrange
    FilterMapper filterMapper = new FilterMapper();
    filterMapper.addFilterReader(new AntFilterReader());

    // Act and Assert
    assertArrayEquals(new String[]{"foo.txt"}, filterMapper.mapFileName("foo.txt"));
  }

  /**
   * Test new {@link FilterMapper} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FilterMapper}
   */
  @Test
  public void testNewFilterMapper() {
    // Arrange and Act
    FilterMapper actualFilterMapper = new FilterMapper();

    // Assert
    Location location = actualFilterMapper.getLocation();
    assertNull(location.getFileName());
    assertNull(actualFilterMapper.getDescription());
    assertNull(actualFilterMapper.getProject());
    assertNull(actualFilterMapper.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualFilterMapper.isReference());
    assertTrue(actualFilterMapper.getFilterReaders().isEmpty());
  }
}
