package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Hashtable;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.junit.Test;
import org.xml.sax.AttributeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributeListImpl;

public class DescriptorHandlerDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link DescriptorHandler#DescriptorHandler(Task, File)}
   *   <li>{@link DescriptorHandler#getEjbName()}
   *   <li>{@link DescriptorHandler#getPublicId()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    TaskAdapter task = new TaskAdapter();

    // Act
    DescriptorHandler actualDescriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    String actualEjbName = actualDescriptorHandler.getEjbName();

    // Assert
    assertNull(actualEjbName);
    assertNull(actualDescriptorHandler.getPublicId());
  }

  /**
   * Test {@link DescriptorHandler#resolveEntity(String, String)}.
   * <p>
   * Method under test: {@link DescriptorHandler#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity() throws SAXException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    DescriptorHandler descriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    InputSource actualResolveEntityResult = descriptorHandler.resolveEntity("42", "42");

    // Assert
    assertEquals("42", descriptorHandler.getPublicId());
    assertNull(actualResolveEntityResult);
  }

  /**
   * Test {@link DescriptorHandler#resolveEntity(String, String)}.
   * <p>
   * Method under test: {@link DescriptorHandler#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity2() throws SAXException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());
    DescriptorHandler descriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    InputSource actualResolveEntityResult = descriptorHandler.resolveEntity("42", "42");

    // Assert
    assertEquals("42", descriptorHandler.getPublicId());
    assertNull(actualResolveEntityResult);
  }

  /**
   * Test {@link DescriptorHandler#resolveEntity(String, String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DescriptorHandler#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_givenJavaLangObject() throws SAXException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(", systemId: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);
    DescriptorHandler descriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    InputSource actualResolveEntityResult = descriptorHandler.resolveEntity("42", "42");

    // Assert
    assertEquals("42", descriptorHandler.getPublicId());
    assertNull(actualResolveEntityResult);
  }

  /**
   * Test {@link DescriptorHandler#resolveEntity(String, String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DescriptorHandler#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_givenProjectAddBuildListenerAntClassLoader() throws SAXException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);
    DescriptorHandler descriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    InputSource actualResolveEntityResult = descriptorHandler.resolveEntity("42", "42");

    // Assert
    assertEquals("42", descriptorHandler.getPublicId());
    assertNull(actualResolveEntityResult);
  }

  /**
   * Test {@link DescriptorHandler#resolveEntity(String, String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link DescriptorHandler#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_givenProjectAddBuildListenerDefaultLogger() throws SAXException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);
    DescriptorHandler descriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    InputSource actualResolveEntityResult = descriptorHandler.resolveEntity("42", "42");

    // Assert
    assertEquals("42", descriptorHandler.getPublicId());
    assertNull(actualResolveEntityResult);
  }

  /**
   * Test {@link DescriptorHandler#getFiles()}.
   * <p>
   * Method under test: {@link DescriptorHandler#getFiles()}
   */
  @Test
  public void testGetFiles() {
    // Arrange
    TaskAdapter task = new TaskAdapter();

    // Act and Assert
    assertTrue(
        (new DescriptorHandler(task, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())).getFiles()
            .isEmpty());
  }

  /**
   * Test {@link DescriptorHandler#startDocument()}.
   * <p>
   * Method under test: {@link DescriptorHandler#startDocument()}
   */
  @Test
  public void testStartDocument() throws SAXException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    DescriptorHandler descriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    descriptorHandler.startDocument();

    // Assert
    Hashtable<String, File> files = descriptorHandler.getFiles();
    assertTrue(files.isEmpty());
    assertTrue(descriptorHandler.ejbFiles.isEmpty());
    assertSame(descriptorHandler.ejbFiles, files);
  }

  /**
   * Test {@link DescriptorHandler#startElement(String, AttributeList)}.
   * <p>
   * Method under test: {@link DescriptorHandler#startElement(String, AttributeList)}
   */
  @Test
  public void testStartElement() throws SAXException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    DescriptorHandler descriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    descriptorHandler.startElement("Name", new AttributeListImpl());

    // Assert
    assertEquals("", descriptorHandler.currentText);
    assertEquals("Name", descriptorHandler.currentElement);
  }

  /**
   * Test {@link DescriptorHandler#startElement(String, AttributeList)}.
   * <p>
   * Method under test: {@link DescriptorHandler#startElement(String, AttributeList)}
   */
  @Test
  public void testStartElement2() throws SAXException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    DescriptorHandler descriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    descriptorHandler.startElement("ejb-ref", new AttributeListImpl());

    // Assert
    assertEquals("", descriptorHandler.currentText);
    assertEquals("ejb-ref", descriptorHandler.currentElement);
  }

  /**
   * Test {@link DescriptorHandler#startElement(String, AttributeList)}.
   * <p>
   * Method under test: {@link DescriptorHandler#startElement(String, AttributeList)}
   */
  @Test
  public void testStartElement3() throws SAXException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    DescriptorHandler descriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    descriptorHandler.startElement("ejb-local-ref", new AttributeListImpl());

    // Assert
    assertEquals("", descriptorHandler.currentText);
    assertEquals("ejb-local-ref", descriptorHandler.currentElement);
  }

  /**
   * Test {@link DescriptorHandler#startElement(String, AttributeList)}.
   * <p>
   * Method under test: {@link DescriptorHandler#startElement(String, AttributeList)}
   */
  @Test
  public void testStartElement4() throws SAXException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    DescriptorHandler descriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    descriptorHandler.startElement("ejb-jar", new AttributeListImpl());

    // Assert
    assertEquals("", descriptorHandler.currentText);
    assertEquals("ejb-jar", descriptorHandler.currentElement);
  }

  /**
   * Test {@link DescriptorHandler#endElement(String)}.
   * <ul>
   *   <li>When {@code ejb-local-ref}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DescriptorHandler#endElement(String)}
   */
  @Test
  public void testEndElement_whenEjbLocalRef() throws SAXException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    DescriptorHandler descriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    descriptorHandler.endElement("ejb-local-ref");

    // Assert
    assertEquals("", descriptorHandler.currentElement);
    assertEquals("", descriptorHandler.currentText);
  }

  /**
   * Test {@link DescriptorHandler#endElement(String)}.
   * <ul>
   *   <li>When {@code ejb-ref}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DescriptorHandler#endElement(String)}
   */
  @Test
  public void testEndElement_whenEjbRef() throws SAXException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    DescriptorHandler descriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    descriptorHandler.endElement("ejb-ref");

    // Assert
    assertEquals("", descriptorHandler.currentElement);
    assertEquals("", descriptorHandler.currentText);
  }

  /**
   * Test {@link DescriptorHandler#endElement(String)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DescriptorHandler#endElement(String)}
   */
  @Test
  public void testEndElement_whenName() throws SAXException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    DescriptorHandler descriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    descriptorHandler.endElement("Name");

    // Assert
    assertEquals("", descriptorHandler.currentElement);
    assertEquals("", descriptorHandler.currentText);
  }

  /**
   * Test {@link DescriptorHandler#characters(char[], int, int)}.
   * <p>
   * Method under test: {@link DescriptorHandler#characters(char[], int, int)}
   */
  @Test
  public void testCharacters() throws SAXException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    DescriptorHandler descriptorHandler = new DescriptorHandler(task,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    descriptorHandler.characters("A.A.".toCharArray(), 1, 3);

    // Assert
    assertEquals("null.A.", descriptorHandler.currentText);
  }
}
