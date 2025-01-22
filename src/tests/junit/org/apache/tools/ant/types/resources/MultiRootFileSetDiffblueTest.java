package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Iterator;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.junit.Test;

public class MultiRootFileSetDiffblueTest {
  /**
   * Test {@link MultiRootFileSet#setDir(File)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultiRootFileSet#setDir(File)}
   */
  @Test
  public void testSetDir_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    MultiRootFileSet multiRootFileSet = new MultiRootFileSet();
    multiRootFileSet.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> multiRootFileSet.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link MultiRootFileSet#setDir(File)}.
   * <ul>
   *   <li>Given {@link MultiRootFileSet} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MultiRootFileSet#setDir(File)}
   */
  @Test
  public void testSetDir_givenMultiRootFileSet() {
    // Arrange
    MultiRootFileSet multiRootFileSet = new MultiRootFileSet();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> multiRootFileSet.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link MultiRootFileSet#setDir(File)}.
   * <ul>
   *   <li>Given {@link MultiRootFileSet} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MultiRootFileSet#setDir(File)}
   */
  @Test
  public void testSetDir_givenMultiRootFileSetProjectIsProject() {
    // Arrange
    MultiRootFileSet multiRootFileSet = new MultiRootFileSet();
    multiRootFileSet.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> multiRootFileSet.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link MultiRootFileSet#setDir(File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultiRootFileSet#setDir(File)}
   */
  @Test
  public void testSetDir_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    MultiRootFileSet multiRootFileSet = new MultiRootFileSet();
    multiRootFileSet.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> multiRootFileSet.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link MultiRootFileSet#setDir(File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@code ant.PropertyHelper} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultiRootFileSet#setDir(File)}
   */
  @Test
  public void testSetDir_givenProjectAddReferenceAntPropertyHelperAndValue() {
    // Arrange
    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    MultiRootFileSet multiRootFileSet = new MultiRootFileSet();
    multiRootFileSet.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> multiRootFileSet.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link MultiRootFileSet#setDir(File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Default is {@code ant.ComponentHelper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultiRootFileSet#setDir(File)}
   */
  @Test
  public void testSetDir_givenProjectDefaultIsAntComponentHelper() {
    // Arrange
    Project project = new Project();
    project.setDefault("ant.ComponentHelper");
    project.addBuildListener(new AntClassLoader());

    MultiRootFileSet multiRootFileSet = new MultiRootFileSet();
    multiRootFileSet.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> multiRootFileSet.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link MultiRootFileSet#setBaseDirs(String)}.
   * <ul>
   *   <li>Given {@link MultiRootFileSet} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link MultiRootFileSet} (default constructor) size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultiRootFileSet#setBaseDirs(String)}
   */
  @Test
  public void testSetBaseDirs_givenMultiRootFileSet_whenNull_thenMultiRootFileSetSizeIsZero() {
    // Arrange
    MultiRootFileSet multiRootFileSet = new MultiRootFileSet();

    // Act
    multiRootFileSet.setBaseDirs(null);

    // Assert that nothing has changed
    assertEquals(0, multiRootFileSet.size());
    assertTrue(multiRootFileSet.isEmpty());
  }

  /**
   * Test {@link MultiRootFileSet#setBaseDirs(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link MultiRootFileSet} (default constructor) size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultiRootFileSet#setBaseDirs(String)}
   */
  @Test
  public void testSetBaseDirs_whenEmptyString_thenMultiRootFileSetSizeIsZero() {
    // Arrange
    MultiRootFileSet multiRootFileSet = new MultiRootFileSet();

    // Act
    multiRootFileSet.setBaseDirs("");

    // Assert that nothing has changed
    assertEquals(0, multiRootFileSet.size());
    assertTrue(multiRootFileSet.isEmpty());
  }

  /**
   * Test {@link MultiRootFileSet#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link MultiRootFileSet} (default constructor).</li>
   *   <li>Then {@link MultiRootFileSet} (default constructor) Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultiRootFileSet#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenMultiRootFileSet_thenMultiRootFileSetReference() {
    // Arrange
    MultiRootFileSet multiRootFileSet = new MultiRootFileSet();
    Reference r = new Reference("42");

    // Act
    multiRootFileSet.setRefid(r);

    // Assert
    assertTrue(multiRootFileSet.isReference());
    assertSame(r, multiRootFileSet.getRefid());
  }

  /**
   * Test {@link MultiRootFileSet#clone()}.
   * <ul>
   *   <li>Given {@link MultiRootFileSet} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MultiRootFileSet#clone()}
   */
  @Test
  public void testClone_givenMultiRootFileSet() {
    // Arrange and Act
    Object actualCloneResult = (new MultiRootFileSet()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof MultiRootFileSet);
    assertNull(((MultiRootFileSet) actualCloneResult).getDir());
    assertNull(((MultiRootFileSet) actualCloneResult).getDescription());
    assertNull(((MultiRootFileSet) actualCloneResult).getProject());
    assertNull(((MultiRootFileSet) actualCloneResult).getRefid());
    assertEquals(0, ((MultiRootFileSet) actualCloneResult).size());
    assertEquals(5, ((MultiRootFileSet) actualCloneResult).getMaxLevelsOfSymlinks());
    assertFalse(((MultiRootFileSet) actualCloneResult).isReference());
    assertTrue(((MultiRootFileSet) actualCloneResult).getDefaultexcludes());
    assertTrue(((MultiRootFileSet) actualCloneResult).getErrorOnMissingDir());
    assertTrue(((MultiRootFileSet) actualCloneResult).isEmpty());
    assertTrue(((MultiRootFileSet) actualCloneResult).isFilesystemOnly());
  }

  /**
   * Test {@link MultiRootFileSet#clone()}.
   * <ul>
   *   <li>Given {@link MultiRootFileSet} (default constructor) appendSelector {@link ScriptSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MultiRootFileSet#clone()}
   */
  @Test
  public void testClone_givenMultiRootFileSetAppendSelectorScriptSelector() {
    // Arrange
    MultiRootFileSet multiRootFileSet = new MultiRootFileSet();
    multiRootFileSet.appendSelector(new ScriptSelector());

    // Act
    Object actualCloneResult = multiRootFileSet.clone();

    // Assert
    assertTrue(actualCloneResult instanceof MultiRootFileSet);
    assertNull(((MultiRootFileSet) actualCloneResult).getDir());
    assertNull(((MultiRootFileSet) actualCloneResult).getDescription());
    assertNull(((MultiRootFileSet) actualCloneResult).getProject());
    assertNull(((MultiRootFileSet) actualCloneResult).getRefid());
    assertEquals(0, ((MultiRootFileSet) actualCloneResult).size());
    assertEquals(5, ((MultiRootFileSet) actualCloneResult).getMaxLevelsOfSymlinks());
    assertFalse(((MultiRootFileSet) actualCloneResult).isReference());
    assertTrue(((MultiRootFileSet) actualCloneResult).getDefaultexcludes());
    assertTrue(((MultiRootFileSet) actualCloneResult).getErrorOnMissingDir());
    assertTrue(((MultiRootFileSet) actualCloneResult).isEmpty());
    assertTrue(((MultiRootFileSet) actualCloneResult).isFilesystemOnly());
  }

  /**
   * Test {@link MultiRootFileSet#iterator()}.
   * <ul>
   *   <li>Given {@link MultiRootFileSet} (default constructor).</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultiRootFileSet#iterator()}
   */
  @Test
  public void testIterator_givenMultiRootFileSet_thenReturnFailFast() {
    // Arrange and Act
    Iterator<Resource> actualIteratorResult = (new MultiRootFileSet()).iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link MultiRootFileSet#size()}.
   * <ul>
   *   <li>Given {@link MultiRootFileSet} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultiRootFileSet#size()}
   */
  @Test
  public void testSize_givenMultiRootFileSet_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new MultiRootFileSet()).size());
  }

  /**
   * Test {@link MultiRootFileSet#isFilesystemOnly()}.
   * <p>
   * Method under test: {@link MultiRootFileSet#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly() {
    // Arrange, Act and Assert
    assertTrue((new MultiRootFileSet()).isFilesystemOnly());
  }

  /**
   * Test {@link MultiRootFileSet#toString()}.
   * <ul>
   *   <li>Given {@link MultiRootFileSet} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultiRootFileSet#toString()}
   */
  @Test
  public void testToString_givenMultiRootFileSet_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new MultiRootFileSet()).toString());
  }

  /**
   * Test new {@link MultiRootFileSet} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MultiRootFileSet}
   */
  @Test
  public void testNewMultiRootFileSet() {
    // Arrange and Act
    MultiRootFileSet actualMultiRootFileSet = new MultiRootFileSet();

    // Assert
    assertNull(actualMultiRootFileSet.getDir());
    assertNull(actualMultiRootFileSet.getDescription());
    assertNull(actualMultiRootFileSet.getProject());
    assertNull(actualMultiRootFileSet.getRefid());
    assertEquals(0, actualMultiRootFileSet.size());
    assertEquals(5, actualMultiRootFileSet.getMaxLevelsOfSymlinks());
    assertFalse(actualMultiRootFileSet.isReference());
    assertTrue(actualMultiRootFileSet.getDefaultexcludes());
    assertTrue(actualMultiRootFileSet.getErrorOnMissingDir());
    assertTrue(actualMultiRootFileSet.isEmpty());
    assertTrue(actualMultiRootFileSet.isFilesystemOnly());
  }
}
