package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class ImportTaskDiffblueTest {
  /**
   * Test new {@link ImportTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ImportTask}
   */
  @Test
  public void testNewImportTask() {
    // Arrange and Act
    ImportTask actualImportTask = new ImportTask();

    // Assert
    Location location = actualImportTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualImportTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualImportTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualImportTask.getTaskName());
    assertNull(actualImportTask.getTaskType());
    assertNull(actualImportTask.getProject());
    assertNull(actualImportTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualImportTask.isInIncludeMode());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualImportTask, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenConcatAddFilelistFileList_thenThrowBuildException() {
    // Arrange
    Concat r = new Concat();
    r.addFilelist(new FileList());

    ImportTask importTask = new ImportTask();
    importTask.add(r);

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code At least one resource must be provided, or some text.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenConcatAddTextAtLeastOneResourceMustBeProvidedOrSomeText() {
    // Arrange
    Concat r = new Concat();
    r.addText("At least one resource must be provided, or some text.");

    ImportTask importTask = new ImportTask();
    importTask.add(r);

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenConcatDestIsResource_thenThrowBuildException() {
    // Arrange
    Concat r = new Concat();
    r.setDest(new Resource());
    r.addFilelist(new FileList());

    ImportTask importTask = new ImportTask();
    importTask.add(r);

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenConcatProjectIsProject_thenThrowBuildException() {
    // Arrange
    Concat r = new Concat();
    r.setProject(new Project());
    r.addText("At least one resource must be provided, or some text.");

    ImportTask importTask = new ImportTask();
    importTask.add(r);

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) ResourceName is {@code concat (}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenConcatResourceNameIsConcat_thenThrowBuildException() {
    // Arrange
    Concat r = new Concat();
    r.setResourceName("concat (");
    r.addFilelist(new FileList());

    ImportTask importTask = new ImportTask();
    importTask.add(r);

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code 42}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenFileNameNameIs42_thenThrowBuildException() {
    // Arrange
    FileName name = new FileName();
    name.setName("42");

    FileList r = new FileList();
    r.addConfiguredFile(name);

    ImportTask importTask = new ImportTask();
    importTask.add(r);

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code 42}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenFileNameNameIs42_thenThrowBuildException2() {
    // Arrange
    FileName name = new FileName();
    name.setName("42");

    FileName name2 = new FileName();
    name2.setName("42");

    FileList r = new FileList();
    r.addConfiguredFile(name2);
    r.addConfiguredFile(name);

    ImportTask importTask = new ImportTask();
    importTask.add(r);

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code concat (}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenFileNameNameIsConcat_thenThrowBuildException() {
    // Arrange
    FileName name = new FileName();
    name.setName("concat (");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat r = new Concat();
    r.addFilelist(list);

    ImportTask importTask = new ImportTask();
    importTask.add(r);

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code ..}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenFileNameNameIsDotDot_thenThrowBuildException() {
    // Arrange
    FileName name = new FileName();
    name.setName("..");

    FileList r = new FileList();
    r.addConfiguredFile(name);

    ImportTask importTask = new ImportTask();
    importTask.add(r);

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code .}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenFileNameNameIsDot_thenThrowBuildException() {
    // Arrange
    FileName name = new FileName();
    name.setName("42");

    FileName name2 = new FileName();
    name2.setName(".");

    FileList r = new FileList();
    r.addConfiguredFile(name2);
    r.addConfiguredFile(name);

    ImportTask importTask = new ImportTask();
    importTask.add(r);

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenFileNameNameIsEmptyString_thenThrowBuildException() {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileList r = new FileList();
    r.addConfiguredFile(name);

    ImportTask importTask = new ImportTask();
    importTask.add(r);

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link ImportTask} (default constructor) add {@link FileList#FileList()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenImportTaskAddFileList_thenThrowBuildException() {
    // Arrange
    ImportTask importTask = new ImportTask();
    importTask.add(new FileList());

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link ImportTask} (default constructor) File is {@code No directory specified for %s.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenImportTaskFileIsNoDirectorySpecifiedForS() {
    // Arrange
    ImportTask importTask = new ImportTask();
    importTask.setFile("No directory specified for %s.");
    importTask.add(Path.systemBootClasspath);

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link ImportTask} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenImportTask_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ImportTask()).execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Concat r = new Concat();
    r.setProject(project);
    r.addText("At least one resource must be provided, or some text.");

    ImportTask importTask = new ImportTask();
    importTask.add(r);

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Concat r = new Concat();
    r.setProject(project);
    r.addText("At least one resource must be provided, or some text.");

    ImportTask importTask = new ImportTask();
    importTask.add(r);

    // Act and Assert
    assertThrows(BuildException.class, () -> importTask.execute());
  }

  /**
   * Test {@link ImportTask#isInIncludeMode()}.
   * <ul>
   *   <li>Given {@link ImportTask} (default constructor) TaskType is {@code include}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#isInIncludeMode()}
   */
  @Test
  public void testIsInIncludeMode_givenImportTaskTaskTypeIsInclude_thenReturnTrue() {
    // Arrange
    ImportTask importTask = new ImportTask();
    importTask.setTaskType("include");

    // Act and Assert
    assertTrue(importTask.isInIncludeMode());
  }

  /**
   * Test {@link ImportTask#isInIncludeMode()}.
   * <ul>
   *   <li>Given {@link ImportTask} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImportTask#isInIncludeMode()}
   */
  @Test
  public void testIsInIncludeMode_givenImportTask_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new ImportTask()).isInIncludeMode());
  }
}
