package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.junit.Test;

public class AbstractResourceCollectionWrapperDiffblueTest {
  /**
   * Test {@link AbstractResourceCollectionWrapper#isCache()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) Cache is {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isCache()}
   */
  @Test
  public void testIsCache_givenAllButFirstCacheIsFalse_thenReturnFalse() {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setCache(false);

    // Act and Assert
    assertFalse(allButFirst.isCache());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isCache()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isCache()}
   */
  @Test
  public void testIsCache_givenAllButFirst_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new AllButFirst()).isCache());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenAllButFirstAddNone_whenNone_thenThrowBuildException() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.add(Resources.NONE));
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) Description is {@code The characteristics of someone or something}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenAllButFirstDescriptionIsTheCharacteristicsOfSomeoneOrSomething() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setDescription("The characteristics of someone or something");
    allButFirst.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.add(Resources.NONE));
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenAllButFirstProjectIsProject_whenNone_thenThrowBuildException() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(new Project());
    allButFirst.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.add(Resources.NONE));
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor).</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then {@link AllButFirst} (default constructor) Collection {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenAllButFirst_whenNone_thenAllButFirstCollectionList() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    ResourceCollection c = Resources.NONE;

    // Act
    allButFirst.add(c);

    // Assert
    Collection<Resource> collection = allButFirst.getCollection();
    assertTrue(collection instanceof List);
    assertEquals(0, allButFirst.size());
    assertEquals(0, allButFirst.getSize());
    assertTrue(collection.isEmpty());
    assertTrue(allButFirst.isEmpty());
    assertSame(c, allButFirst.getResourceCollection());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenJavaLangObject_whenNone_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);
    allButFirst.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.add(Resources.NONE));
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);
    allButFirst.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.add(Resources.NONE));
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@code ant.PropertyHelper} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenProjectAddReferenceAntPropertyHelperAndValue() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);
    allButFirst.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.add(Resources.NONE));
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Default is {@code ant.ComponentHelper}.</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenProjectDefaultIsAntComponentHelper_whenNone_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setDefault("ant.ComponentHelper");
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);
    allButFirst.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.add(Resources.NONE));
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}.
   * <ul>
   *   <li>Then {@link AllButFirst} (default constructor) Project BuildListeners Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#add(ResourceCollection)}
   */
  @Test
  public void testAdd_thenAllButFirstProjectBuildListenersEmpty() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(new Project());
    ResourceCollection c = Resources.NONE;

    // Act
    allButFirst.add(c);

    // Assert
    Collection<Resource> collection = allButFirst.getCollection();
    assertTrue(collection instanceof List);
    assertEquals(0, allButFirst.size());
    assertEquals(0, allButFirst.getSize());
    assertTrue(collection.isEmpty());
    assertTrue(allButFirst.getProject().getBuildListeners().isEmpty());
    assertTrue(allButFirst.isEmpty());
    assertSame(c, allButFirst.getResourceCollection());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link AllButFirst} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenAllButFirstAddAllButFirst_thenThrowBuildException() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(new AllButFirst());

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.iterator());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link FileList#FileList()}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenAllButFirstAddFileList_thenReturnFailFast() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(new FileList());

    // Act
    Iterator<Resource> actualIteratorResult = allButFirst.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenAllButFirstAddNone_thenReturnFailFast() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(Resources.NONE);

    // Act
    Iterator<Resource> actualIteratorResult = allButFirst.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) Description is {@code The characteristics of someone or something}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenAllButFirstDescriptionIsTheCharacteristicsOfSomeoneOrSomething() {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setDescription("The characteristics of someone or something");

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.iterator());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenAllButFirstProjectIsProject_thenThrowBuildException() {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.iterator());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenAllButFirst_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new AllButFirst()).iterator());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenConcatAddFilelistFileList_thenReturnFailFast() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addFilelist(new FileList());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = allButFirst.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code At least one resource must be provided, or some text.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenConcatAddTextAtLeastOneResourceMustBeProvidedOrSomeText() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addText("At least one resource must be provided, or some text.");

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = allButFirst.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenConcatDestIsResource_thenReturnFailFast() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource());
    c.addFilelist(new FileList());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = allButFirst.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenConcatProjectIsProject_thenReturnFailFast() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setProject(new Project());
    c.addText("At least one resource must be provided, or some text.");

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = allButFirst.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenJavaLangObject_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.iterator());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@code AllButFirst}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenOrgApacheToolsAntTypesResourcesAllButFirst() {
    // Arrange
    Project project = new Project();
    Class<AllButFirst> typeClass = AllButFirst.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.iterator());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.iterator());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenProjectAddBuildListenerDefaultLogger_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.iterator());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#iterator()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@code ant.PropertyHelper} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#iterator()}
   */
  @Test
  public void testIterator_givenProjectAddReferenceAntPropertyHelperAndValue() {
    // Arrange
    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.iterator());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#size()}.
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#size()}
   */
  @Test
  public void testSize() {
    // Arrange
    LazyResourceCollectionWrapper lazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();
    lazyResourceCollectionWrapper.setDescription("The characteristics of someone or something");

    // Act and Assert
    assertThrows(BuildException.class, () -> lazyResourceCollectionWrapper.size());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#size()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#size()}
   */
  @Test
  public void testSize_givenAllButFirst_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new AllButFirst()).size());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#size()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#size()}
   */
  @Test
  public void testSize_givenJavaLangObject_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    LazyResourceCollectionWrapper lazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();
    lazyResourceCollectionWrapper.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> lazyResourceCollectionWrapper.size());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#size()}.
   * <ul>
   *   <li>Given {@link LazyResourceCollectionWrapper} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#size()}
   */
  @Test
  public void testSize_givenLazyResourceCollectionWrapperAddNone_thenReturnZero() throws BuildException {
    // Arrange
    LazyResourceCollectionWrapper lazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();
    lazyResourceCollectionWrapper.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, lazyResourceCollectionWrapper.size());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#size()}.
   * <ul>
   *   <li>Given {@link LazyResourceCollectionWrapper} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#size()}
   */
  @Test
  public void testSize_givenLazyResourceCollectionWrapperProjectIsProject() {
    // Arrange
    LazyResourceCollectionWrapper lazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();
    lazyResourceCollectionWrapper.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> lazyResourceCollectionWrapper.size());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#size()}.
   * <ul>
   *   <li>Given {@link LazyResourceCollectionWrapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#size()}
   */
  @Test
  public void testSize_givenLazyResourceCollectionWrapper_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new LazyResourceCollectionWrapper()).size());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#size()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#size()}
   */
  @Test
  public void testSize_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    LazyResourceCollectionWrapper lazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();
    lazyResourceCollectionWrapper.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> lazyResourceCollectionWrapper.size());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#size()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@code ant.PropertyHelper} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#size()}
   */
  @Test
  public void testSize_givenProjectAddReferenceAntPropertyHelperAndValue() {
    // Arrange
    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    LazyResourceCollectionWrapper lazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();
    lazyResourceCollectionWrapper.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> lazyResourceCollectionWrapper.size());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    c.addFilelist(new FileList());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link AllButFirst} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenAllButFirstAddAllButFirst_thenReturnTrue() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(new AllButFirst());

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Archives} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenAllButFirstAddArchives_thenReturnTrue() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(new Archives());

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenAllButFirstAddBZip2Resource_thenReturnTrue() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(new BZip2Resource());

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link FileResource#FileResource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenAllButFirstAddFileResource_thenReturnTrue() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(new FileResource());

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenAllButFirstAddNone_thenReturnTrue() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(Resources.NONE);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenAllButFirst_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new AllButFirst()).isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenConcatAddFilelistFileList_thenReturnTrue() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addFilelist(new FileList());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code cr}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenConcatAddTextCr_thenReturnTrue() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addText("cr");

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link FileResource#FileResource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenConcatDestIsFileResource_thenReturnTrue() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new FileResource());
    c.addFilelist(new FileList());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource(String)} with name is {@code No directory specified for %s.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenConcatDestIsResourceWithNameIsNoDirectorySpecifiedForS() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource("No directory specified for %s."));
    c.addFilelist(new FileList());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenConcatDestIsResource_thenReturnTrue() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource());
    c.addFilelist(new FileList());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenConcatProjectIsProject_thenReturnTrue() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setProject(new Project());
    c.addText("cr");

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code ..}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFileNameNameIsDotDot_thenReturnTrue() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("..");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.addFilelist(list);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code .}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFileNameNameIsDot_thenReturnTrue() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName(".");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.addFilelist(list);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFileNameNameIsEmptyString_thenReturnTrue() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.addFilelist(list);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code Name}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFileNameNameIsName_thenReturnTrue() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.addFilelist(list);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code Name}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFileNameNameIsName_thenReturnTrue2() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.setDest(new Resource());
    c.addFilelist(list);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code Name}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFileNameNameIsName_thenReturnTrue3() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileName name2 = new FileName();
    name2.setName("Name");

    FileList list = new FileList();
    list.addConfiguredFile(name2);
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.setDest(new Resource());
    c.addFilelist(list);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code Users}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFileNameNameIsUsers_thenReturnTrue() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileName name2 = new FileName();
    name2.setName("Users");

    FileList list = new FileList();
    list.addConfiguredFile(name2);
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.addFilelist(list);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenProjectAddBuildListenerAntClassLoader_thenReturnTrue() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Concat c = new Concat();
    c.setProject(project);
    c.addText("cr");

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertTrue(allButFirst.isFilesystemOnly());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#getResourceCollection()}.
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#getResourceCollection()}
   */
  @Test
  public void testGetResourceCollection() {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setDescription("The characteristics of someone or something");

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.getResourceCollection());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#getResourceCollection()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#getResourceCollection()}
   */
  @Test
  public void testGetResourceCollection_givenAllButFirstAddNone_thenReturnEmpty() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(Resources.NONE);

    // Act and Assert
    assertTrue(allButFirst.getResourceCollection().isEmpty());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#getResourceCollection()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#getResourceCollection()}
   */
  @Test
  public void testGetResourceCollection_givenAllButFirstProjectIsProject() {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.getResourceCollection());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#getResourceCollection()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#getResourceCollection()}
   */
  @Test
  public void testGetResourceCollection_givenAllButFirst_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new AllButFirst()).getResourceCollection());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#getResourceCollection()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#getResourceCollection()}
   */
  @Test
  public void testGetResourceCollection_givenJavaLangObject_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.getResourceCollection());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#getResourceCollection()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#getResourceCollection()}
   */
  @Test
  public void testGetResourceCollection_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.getResourceCollection());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#getResourceCollection()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@code ant.PropertyHelper} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#getResourceCollection()}
   */
  @Test
  public void testGetResourceCollection_givenProjectAddReferenceAntPropertyHelperAndValue() {
    // Arrange
    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.getResourceCollection());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#getResourceCollection()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Default is {@code ant.ComponentHelper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#getResourceCollection()}
   */
  @Test
  public void testGetResourceCollection_givenProjectDefaultIsAntComponentHelper() {
    // Arrange
    Project project = new Project();
    project.setDefault("ant.ComponentHelper");
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.getResourceCollection());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#toString()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link AllButFirst} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#toString()}
   */
  @Test
  public void testToString_givenAllButFirstAddAllButFirst_thenThrowBuildException() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(new AllButFirst());

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.toString());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#toString()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Archives} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#toString()}
   */
  @Test
  public void testToString_givenAllButFirstAddArchives_thenReturnEmptyString() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(new Archives());

    // Act and Assert
    assertEquals("", allButFirst.toString());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#toString()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#toString()}
   */
  @Test
  public void testToString_givenAllButFirstAddBZip2Resource_thenReturnEmptyString() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(new BZip2Resource());

    // Act and Assert
    assertEquals("", allButFirst.toString());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#toString()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#toString()}
   */
  @Test
  public void testToString_givenAllButFirstAddNone_thenReturnEmptyString() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(Resources.NONE);

    // Act and Assert
    assertEquals("", allButFirst.toString());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#toString()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) Description is {@code The characteristics of someone or something}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#toString()}
   */
  @Test
  public void testToString_givenAllButFirstDescriptionIsTheCharacteristicsOfSomeoneOrSomething() {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setDescription("The characteristics of someone or something");

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.toString());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#toString()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#toString()}
   */
  @Test
  public void testToString_givenAllButFirstProjectIsProject_thenThrowBuildException() {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.toString());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#toString()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#toString()}
   */
  @Test
  public void testToString_givenAllButFirst_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new AllButFirst()).toString());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#toString()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#toString()}
   */
  @Test
  public void testToString_givenJavaLangObject_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.toString());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#toString()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#toString()}
   */
  @Test
  public void testToString_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.toString());
  }

  /**
   * Test {@link AbstractResourceCollectionWrapper#toString()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@code ant.PropertyHelper} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractResourceCollectionWrapper#toString()}
   */
  @Test
  public void testToString_givenProjectAddReferenceAntPropertyHelperAndValue() {
    // Arrange
    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.toString());
  }
}
