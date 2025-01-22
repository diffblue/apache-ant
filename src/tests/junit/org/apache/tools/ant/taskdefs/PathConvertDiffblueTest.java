package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.PathConvert.MapEntry;
import org.apache.tools.ant.taskdefs.PathConvert.TargetOs;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.junit.Test;

public class PathConvertDiffblueTest {
  /**
   * Test {@link PathConvert#createPath()}.
   * <ul>
   *   <li>Given {@link PathConvert} (default constructor) add {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PathConvert#createPath()}
   */
  @Test
  public void testCreatePath_givenPathConvertAddSystemBootClasspath() {
    // Arrange
    PathConvert pathConvert = new PathConvert();
    pathConvert.add(Path.systemBootClasspath);

    // Act
    Path actualCreatePathResult = pathConvert.createPath();

    // Assert
    Location location = actualCreatePathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreatePathResult.getDescription());
    assertNull(actualCreatePathResult.getProject());
    assertNull(actualCreatePathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreatePathResult.size());
    assertFalse(actualCreatePathResult.isReference());
    assertTrue(actualCreatePathResult.isEmpty());
  }

  /**
   * Test {@link PathConvert#createPath()}.
   * <ul>
   *   <li>Given {@link PathConvert} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PathConvert#createPath()}
   */
  @Test
  public void testCreatePath_givenPathConvert_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreatePathResult = (new PathConvert()).createPath();

    // Assert
    Location location = actualCreatePathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreatePathResult.getDescription());
    assertNull(actualCreatePathResult.getProject());
    assertNull(actualCreatePathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreatePathResult.size());
    assertFalse(actualCreatePathResult.isReference());
    assertTrue(actualCreatePathResult.isEmpty());
  }

  /**
   * Test {@link PathConvert#createPath()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PathConvert#createPath()}
   */
  @Test
  public void testCreatePath_thenThrowBuildException() {
    // Arrange
    PathConvert pathConvert = new PathConvert();
    pathConvert.setRefid(new Reference("42"));

    // Act and Assert
    assertThrows(BuildException.class, () -> pathConvert.createPath());
  }

  /**
   * Test {@link PathConvert#add(FileNameMapper)} with {@code fileNameMapper}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PathConvert#add(FileNameMapper)}
   */
  @Test
  public void testAddWithFileNameMapper_thenThrowBuildException() {
    // Arrange
    PathConvert pathConvert = new PathConvert();
    pathConvert.addMapper(new Mapper(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> pathConvert.add(new CutDirsMapper()));
  }

  /**
   * Test {@link PathConvert#add(ResourceCollection)} with {@code rc}.
   * <ul>
   *   <li>Given {@link PathConvert} (default constructor) Refid is {@link Reference#Reference(String)} with id is {@code 42}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PathConvert#add(ResourceCollection)}
   */
  @Test
  public void testAddWithRc_givenPathConvertRefidIsReferenceWithIdIs42_thenThrowBuildException() {
    // Arrange
    PathConvert pathConvert = new PathConvert();
    pathConvert.setRefid(new Reference("42"));

    // Act and Assert
    assertThrows(BuildException.class, () -> pathConvert.add(Path.systemBootClasspath));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link PathConvert#setDirSep(String)}
   *   <li>{@link PathConvert#setPathSep(String)}
   *   <li>{@link PathConvert#setPreserveDuplicates(boolean)}
   *   <li>{@link PathConvert#setProperty(String)}
   *   <li>{@link PathConvert#setSetonempty(boolean)}
   *   <li>{@link PathConvert#isPreserveDuplicates()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    PathConvert pathConvert = new PathConvert();

    // Act
    pathConvert.setDirSep("Sep");
    pathConvert.setPathSep("Sep");
    pathConvert.setPreserveDuplicates(true);
    pathConvert.setProperty("foo");
    pathConvert.setSetonempty(true);

    // Assert
    assertTrue(pathConvert.isPreserveDuplicates());
  }

  /**
   * Test {@link PathConvert#addMapper(Mapper)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PathConvert#addMapper(Mapper)}
   */
  @Test
  public void testAddMapper_thenThrowBuildException() {
    // Arrange
    PathConvert pathConvert = new PathConvert();
    pathConvert.addMapper(new Mapper(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> pathConvert.addMapper(new Mapper(new Project())));
  }

  /**
   * Test MapEntry {@link MapEntry#apply(String)}.
   * <ul>
   *   <li>Given {@link MapEntry#MapEntry(PathConvert)} with this$0 is {@link PathConvert} (default constructor) From is {@code foo}.</li>
   *   <li>Then return {@code Elem}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MapEntry#apply(String)}
   */
  @Test
  public void testMapEntryApply_givenMapEntryWithThis$0IsPathConvertFromIsFoo_thenReturnElem() {
    // Arrange
    MapEntry mapEntry = (new PathConvert()).new MapEntry();
    mapEntry.setFrom("foo");
    mapEntry.setTo("foo");

    // Act and Assert
    assertEquals("Elem", mapEntry.apply("Elem"));
  }

  /**
   * Test MapEntry {@link MapEntry#apply(String)}.
   * <ul>
   *   <li>Given {@link MapEntry#MapEntry(PathConvert)} with this$0 is {@link PathConvert} (default constructor) To is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MapEntry#apply(String)}
   */
  @Test
  public void testMapEntryApply_givenMapEntryWithThis$0IsPathConvertToIsNull() {
    // Arrange
    MapEntry mapEntry = (new PathConvert()).new MapEntry();
    mapEntry.setFrom("foo");
    mapEntry.setTo(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> mapEntry.apply("Elem"));
  }

  /**
   * Test MapEntry {@link MapEntry#apply(String)}.
   * <ul>
   *   <li>Given {@link MapEntry#MapEntry(PathConvert)} with this$0 is {@link PathConvert} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MapEntry#apply(String)}
   */
  @Test
  public void testMapEntryApply_givenMapEntryWithThis$0IsPathConvert_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new PathConvert()).new MapEntry()).apply("Elem"));
  }

  /**
   * Test MapEntry {@link MapEntry#apply(String)}.
   * <ul>
   *   <li>Then return {@code fooElem}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MapEntry#apply(String)}
   */
  @Test
  public void testMapEntryApply_thenReturnFooElem() {
    // Arrange
    MapEntry mapEntry = (new PathConvert()).new MapEntry();
    mapEntry.setFrom("");
    mapEntry.setTo("foo");

    // Act and Assert
    assertEquals("fooElem", mapEntry.apply("Elem"));
  }

  /**
   * Test new {@link PathConvert} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link PathConvert}
   */
  @Test
  public void testNewPathConvert() {
    // Arrange and Act
    PathConvert actualPathConvert = new PathConvert();

    // Assert
    Location location = actualPathConvert.getLocation();
    assertNull(location.getFileName());
    assertNull(actualPathConvert.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualPathConvert.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualPathConvert.getTaskName());
    assertNull(actualPathConvert.getTaskType());
    assertNull(actualPathConvert.getProject());
    assertNull(actualPathConvert.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualPathConvert.isPreserveDuplicates());
    assertFalse(actualPathConvert.isReference());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualPathConvert, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test TargetOs {@link TargetOs#getValues()}.
   * <p>
   * Method under test: {@link TargetOs#getValues()}
   */
  @Test
  public void testTargetOsGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"windows", "unix", "netware", "os/2", "tandem"}, (new TargetOs()).getValues());
  }

  /**
   * Test TargetOs new {@link TargetOs} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link TargetOs}
   */
  @Test
  public void testTargetOsNewTargetOs() {
    // Arrange and Act
    TargetOs actualTargetOs = new TargetOs();

    // Assert
    assertNull(actualTargetOs.getValue());
    assertEquals(-1, actualTargetOs.getIndex());
  }
}
