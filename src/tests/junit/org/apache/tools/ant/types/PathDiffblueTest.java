package org.apache.tools.ant.types;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.nio.file.Paths;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildListener;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.listener.BigProjectLogger;
import org.apache.tools.ant.types.Path.PathElement;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.FileResourceIterator;
import org.junit.Test;

public class PathDiffblueTest {
  /**
   * Test PathElement getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link PathElement#PathElement(Path)}
   *   <li>{@link PathElement#getParts()}
   *   <li>{@link PathElement#isFilesystemOnly()}
   * </ul>
   */
  @Test
  public void testPathElementGettersAndSetters() {
    // Arrange and Act
    PathElement actualPathElement = Path.systemBootClasspath.new PathElement();
    String[] actualParts = actualPathElement.getParts();

    // Assert
    assertNull(actualParts);
    assertTrue(actualPathElement.isFilesystemOnly());
  }

  /**
   * Test PathElement {@link PathElement#setLocation(File)}.
   * <p>
   * Method under test: {@link PathElement#setLocation(File)}
   */
  @Test
  public void testPathElementSetLocation() {
    // Arrange
    PathElement pathElement = Path.systemBootClasspath.new PathElement();

    // Act
    pathElement.setLocation(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Iterator<Resource> iteratorResult = pathElement.iterator();
    assertTrue(iteratorResult.next() instanceof FileResource);
    assertTrue(iteratorResult instanceof FileResourceIterator);
    assertEquals(1, pathElement.size());
    assertFalse(iteratorResult.hasNext());
    assertFalse(pathElement.isEmpty());
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString()},
        pathElement.getParts());
  }

  /**
   * Test PathElement {@link PathElement#setPath(String)}.
   * <p>
   * Method under test: {@link PathElement#setPath(String)}
   */
  @Test
  public void testPathElementSetPath() {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);
    PathElement pathElement = (new Path(project)).new PathElement();

    // Act
    pathElement.setPath("Path");

    // Assert
    Iterator<Resource> iteratorResult = pathElement.iterator();
    Resource nextResult = iteratorResult.next();
    assertTrue(nextResult instanceof FileResource);
    assertTrue(iteratorResult instanceof FileResourceIterator);
    Vector<BuildListener> buildListeners = nextResult.getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertFalse(iteratorResult.hasNext());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test PathElement {@link PathElement#setPath(String)}.
   * <p>
   * Method under test: {@link PathElement#setPath(String)}
   */
  @Test
  public void testPathElementSetPath2() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("windows", typeClass);
    project.addBuildListener(new AntClassLoader());
    PathElement pathElement = (new Path(project)).new PathElement();

    // Act
    pathElement.setPath("Path");

    // Assert
    Iterator<Resource> iteratorResult = pathElement.iterator();
    Resource nextResult = iteratorResult.next();
    assertTrue(nextResult instanceof FileResource);
    assertTrue(iteratorResult instanceof FileResourceIterator);
    Project project2 = nextResult.getProject();
    Hashtable<String, Class<?>> dataTypeDefinitions = project2.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    assertFalse(iteratorResult.hasNext());
    assertTrue(dataTypeDefinitions.containsKey("windows"));
    assertTrue(copyOfDataTypeDefinitions.containsKey("windows"));
  }

  /**
   * Test PathElement {@link PathElement#setPath(String)}.
   * <p>
   * Method under test: {@link PathElement#setPath(String)}
   */
  @Test
  public void testPathElementSetPath3() {
    // Arrange
    BigProjectLogger listener = new BigProjectLogger();
    listener.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(1)));

    Project project = new Project();
    project.addBuildListener(listener);
    PathElement pathElement = (new Path(project)).new PathElement();

    // Act
    pathElement.setPath("Path");

    // Assert
    Iterator<Resource> iteratorResult = pathElement.iterator();
    Resource nextResult = iteratorResult.next();
    Vector<BuildListener> buildListeners = nextResult.getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    BuildListener getResult = buildListeners.get(0);
    assertTrue(getResult instanceof BigProjectLogger);
    assertTrue(nextResult instanceof FileResource);
    assertTrue(iteratorResult instanceof FileResourceIterator);
    assertFalse(iteratorResult.hasNext());
    assertSame(listener, getResult);
  }

  /**
   * Test PathElement {@link PathElement#setPath(String)}.
   * <ul>
   *   <li>Then {@link PathElement#PathElement(Path)} with this$0 is {@link Path#Path(Project)} iterator next Name is {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PathElement#setPath(String)}
   */
  @Test
  public void testPathElementSetPath_thenPathElementWithThis$0IsPathIteratorNextNameIsPath() {
    // Arrange
    PathElement pathElement = (new Path(null)).new PathElement();

    // Act
    pathElement.setPath("Path");

    // Assert
    Iterator<Resource> iteratorResult = pathElement.iterator();
    Resource nextResult = iteratorResult.next();
    assertTrue(nextResult instanceof FileResource);
    assertTrue(iteratorResult instanceof FileResourceIterator);
    assertEquals("Path", nextResult.getName());
    assertNull(nextResult.getProject());
    assertFalse(iteratorResult.hasNext());
    assertFalse(nextResult.isDirectory());
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("user.dir"), "Path").toString(), "\"");
    assertEquals(expectedToLongStringResult, nextResult.toLongString());
    assertEquals(Resource.UNKNOWN_DATETIME, nextResult.getLastModified());
    assertEquals(Resource.UNKNOWN_DATETIME, nextResult.getSize());
    assertArrayEquals(new String[]{Paths.get(System.getProperty("user.dir"), "Path").toString()},
        pathElement.getParts());
  }

  /**
   * Test PathElement {@link PathElement#size()}.
   * <ul>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PathElement#size()}
   */
  @Test
  public void testPathElementSize_thenReturnOne() {
    // Arrange
    PathElement pathElement = Path.systemBootClasspath.new PathElement();
    pathElement.setLocation(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(1, pathElement.size());
  }

  /**
   * Test PathElement {@link PathElement#size()}.
   * <ul>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link PathElement#size()}
   */
  @Test
  public void testPathElementSize_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (Path.systemBootClasspath.new PathElement()).size());
  }
}
