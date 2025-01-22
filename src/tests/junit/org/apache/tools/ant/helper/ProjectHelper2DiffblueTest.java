package org.apache.tools.ant.helper;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Paths;
import java.util.Hashtable;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.ExecutorTest;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.UnknownElement;
import org.apache.tools.ant.helper.ProjectHelper2.AntHandler;
import org.apache.tools.ant.helper.ProjectHelper2.ElementHandler;
import org.apache.tools.ant.helper.ProjectHelper2.MainHandler;
import org.apache.tools.ant.helper.ProjectHelper2.ProjectHandler;
import org.apache.tools.ant.helper.ProjectHelper2.RootHandler;
import org.apache.tools.ant.helper.ProjectHelper2.TargetHandler;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;
import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;
import org.xml.sax.ext.Attributes2Impl;
import org.xml.sax.ext.Locator2Impl;

public class ProjectHelper2DiffblueTest {
  /**
   * Test AntHandler {@link AntHandler#characters(char[], int, int, AntXMLContext)}.
   * <ul>
   *   <li>Given {@link MainHandler} (default constructor).</li>
   *   <li>Then throw {@link SAXParseException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntHandler#characters(char[], int, int, AntXMLContext)}
   */
  @Test
  public void testAntHandlerCharacters_givenMainHandler_thenThrowSAXParseException() throws SAXParseException {
    // Arrange
    MainHandler mainHandler = new MainHandler();
    char[] buf = "AZAZ".toCharArray();

    // Act and Assert
    assertThrows(SAXParseException.class, () -> mainHandler.characters(buf, 1, 3, new AntXMLContext(new Project())));
  }

  /**
   * Test AntHandler {@link AntHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>Given {@link AntHandler} (default constructor).</li>
   *   <li>Then throw {@link SAXParseException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testAntHandlerOnStartChild_givenAntHandler_thenThrowSAXParseException() throws SAXParseException {
    // Arrange
    AntHandler antHandler = new AntHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    // Act and Assert
    assertThrows(SAXParseException.class,
        () -> antHandler.onStartChild("Uri", "Tag", "Qname", attrs, new AntXMLContext(new Project())));
  }

  /**
   * Test AntHandler {@link AntHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>Given {@link Locator2Impl#Locator2Impl()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testAntHandlerOnStartChild_givenLocator2Impl() throws SAXParseException {
    // Arrange
    AntHandler elementHandler = ProjectHelper2.getElementHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    AntXMLContext context = new AntXMLContext(new Project());
    context.setLocator(new Locator2Impl());

    // Act and Assert
    assertSame(elementHandler, elementHandler.onStartChild("Uri", "Tag", "Qname", attrs, context));
  }

  /**
   * Test AntHandler {@link AntHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>Given {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testAntHandlerOnStartChild_givenNull() throws SAXParseException {
    // Arrange
    AntHandler elementHandler = ProjectHelper2.getElementHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    AntXMLContext context = new AntXMLContext(new Project());
    context.setLocator(null);

    // Act and Assert
    assertSame(elementHandler, elementHandler.onStartChild("Uri", "Tag", "Qname", attrs, context));
  }

  /**
   * Test AntHandler {@link AntHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>When {@link AntXMLContext#AntXMLContext(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testAntHandlerOnStartChild_whenAntXMLContextWithProjectIsProject() throws SAXParseException {
    // Arrange
    AntHandler elementHandler = ProjectHelper2.getElementHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    // Act and Assert
    assertSame(elementHandler,
        elementHandler.onStartChild("Uri", "Tag", "Qname", attrs, new AntXMLContext(new Project())));
  }

  /**
   * Test {@link ProjectHelper2#canParseAntlibDescriptor(Resource)}.
   * <p>
   * Method under test: {@link ProjectHelper2#canParseAntlibDescriptor(Resource)}
   */
  @Test
  public void testCanParseAntlibDescriptor() {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    // Act and Assert
    assertTrue(projectHelper2.canParseAntlibDescriptor(new Resource()));
  }

  /**
   * Test ElementHandler {@link ElementHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}.
   * <p>
   * Method under test: {@link ElementHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testElementHandlerOnStartChild() throws SAXParseException {
    // Arrange
    ElementHandler elementHandler = new ElementHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    // Act and Assert
    assertTrue(elementHandler.onStartChild("Uri", "Tag", "Qname", attrs,
        new AntXMLContext(new Project())) instanceof ElementHandler);
  }

  /**
   * Test ElementHandler {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <p>
   * Method under test: {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testElementHandlerOnStartElement() throws SAXParseException {
    // Arrange
    ElementHandler elementHandler = new ElementHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    Locator2Impl locator = new Locator2Impl();
    locator.setSystemId("ant:current");

    AntXMLContext context = new AntXMLContext(new Project());
    context.setCurrentTarget(new Target());
    context.setLocator(locator);

    // Act
    elementHandler.onStartElement("antlib:org.apache.tools.ant", "Tag", "Qname", attrs, context);

    // Assert
    Task[] tasks = context.getCurrentTarget().getTasks();
    Task task = tasks[0];
    assertTrue(task instanceof UnknownElement);
    assertEquals("Tag", task.getTaskType());
    assertEquals("antlib:org.apache.tools.ant", ((UnknownElement) task).getNamespace());
    Vector<RuntimeConfigurable> wrapperStack = context.getWrapperStack();
    assertEquals(1, wrapperStack.size());
    assertEquals(1, tasks.length);
    assertTrue(wrapperStack.get(0).getAttributeMap().isEmpty());
  }

  /**
   * Test ElementHandler {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <p>
   * Method under test: {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testElementHandlerOnStartElement2() throws SAXParseException {
    // Arrange
    ElementHandler elementHandler = new ElementHandler();

    Attributes2Impl attrs = new Attributes2Impl();
    attrs.addAttribute("antlib:org.apache.tools.ant", "antlib:org.apache.tools.ant", "antlib:org.apache.tools.ant",
        "antlib:org.apache.tools.ant", "antlib:org.apache.tools.ant");

    Locator2Impl locator = new Locator2Impl();
    locator.setSystemId("ant:current");

    AntXMLContext context = new AntXMLContext(new Project());
    context.setCurrentTarget(new Target());
    context.setLocator(locator);

    // Act
    elementHandler.onStartElement("Uri", "Tag", "Qname", attrs, context);

    // Assert
    Task[] tasks = context.getCurrentTarget().getTasks();
    Task task = tasks[0];
    assertTrue(task instanceof UnknownElement);
    assertEquals("Uri", ((UnknownElement) task).getNamespace());
    assertEquals("Uri:Tag", task.getTaskType());
    Vector<RuntimeConfigurable> wrapperStack = context.getWrapperStack();
    assertEquals(1, wrapperStack.size());
    Hashtable<String, Object> attributeMap = wrapperStack.get(0).getAttributeMap();
    assertEquals(1, attributeMap.size());
    assertEquals("antlib:org.apache.tools.ant",
        attributeMap.get("antlib:org.apache.tools.ant:antlib:org.apache.tools.ant"));
    assertEquals(1, tasks.length);
  }

  /**
   * Test ElementHandler {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <p>
   * Method under test: {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testElementHandlerOnStartElement3() throws SAXParseException {
    // Arrange
    ElementHandler elementHandler = new ElementHandler();

    Attributes2Impl attrs = new Attributes2Impl();
    attrs.addAttribute("ant:current", "ant:current", "ant:current", "ant:current", "ant:current");
    attrs.addAttribute("antlib:org.apache.tools.ant", "antlib:org.apache.tools.ant", "antlib:org.apache.tools.ant",
        "antlib:org.apache.tools.ant", "antlib:org.apache.tools.ant");

    Locator2Impl locator = new Locator2Impl();
    locator.setSystemId("ant:current");

    AntXMLContext context = new AntXMLContext(new Project());
    context.setCurrentTarget(new Target());
    context.setLocator(locator);

    // Act
    elementHandler.onStartElement("Uri", "Tag", "Qname", attrs, context);

    // Assert
    Vector<RuntimeConfigurable> wrapperStack = context.getWrapperStack();
    assertEquals(1, wrapperStack.size());
    Hashtable<String, Object> attributeMap = wrapperStack.get(0).getAttributeMap();
    assertEquals(2, attributeMap.size());
    assertEquals("ant:current", attributeMap.get("ant:current:ant:current"));
    assertEquals("antlib:org.apache.tools.ant",
        attributeMap.get("antlib:org.apache.tools.ant:antlib:org.apache.tools.ant"));
  }

  /**
   * Test ElementHandler {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <p>
   * Method under test: {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testElementHandlerOnStartElement4() throws SAXParseException {
    // Arrange
    ElementHandler elementHandler = new ElementHandler();

    Attributes2Impl attrs = new Attributes2Impl();
    attrs.addAttribute("Uri", "Uri", "Uri", "Uri", "Uri");
    attrs.addAttribute("ant:current", "ant:current", "ant:current", "ant:current", "ant:current");
    attrs.addAttribute("antlib:org.apache.tools.ant", "antlib:org.apache.tools.ant", "antlib:org.apache.tools.ant",
        "antlib:org.apache.tools.ant", "antlib:org.apache.tools.ant");

    Locator2Impl locator = new Locator2Impl();
    locator.setSystemId("ant:current");

    AntXMLContext context = new AntXMLContext(new Project());
    context.setCurrentTarget(new Target());
    context.setLocator(locator);

    // Act
    elementHandler.onStartElement("Uri", "Tag", "Qname", attrs, context);

    // Assert
    Vector<RuntimeConfigurable> wrapperStack = context.getWrapperStack();
    assertEquals(1, wrapperStack.size());
    Hashtable<String, Object> attributeMap = wrapperStack.get(0).getAttributeMap();
    assertEquals(3, attributeMap.size());
    assertEquals("Uri", attributeMap.get("Uri"));
    assertEquals("ant:current", attributeMap.get("ant:current:ant:current"));
    assertTrue(attributeMap.containsKey("antlib:org.apache.tools.ant:antlib:org.apache.tools.ant"));
  }

  /**
   * Test ElementHandler {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testElementHandlerOnStartElement_givenAntClassLoader() throws SAXParseException {
    // Arrange
    ElementHandler elementHandler = new ElementHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Locator2Impl locator = new Locator2Impl();
    locator.setSystemId("ant:current");

    AntXMLContext context = new AntXMLContext(project);
    context.setCurrentTarget(new Target());
    context.setLocator(locator);

    // Act
    elementHandler.onStartElement("ant:current", "Tag", "Qname", attrs, context);

    // Assert
    Task[] tasks = context.getCurrentTarget().getTasks();
    Task task = tasks[0];
    assertTrue(task instanceof UnknownElement);
    assertEquals("", ((UnknownElement) task).getNamespace());
    assertEquals("Tag", task.getTaskType());
    Vector<RuntimeConfigurable> wrapperStack = context.getWrapperStack();
    assertEquals(1, wrapperStack.size());
    assertEquals(1, tasks.length);
    assertTrue(wrapperStack.get(0).getAttributeMap().isEmpty());
  }

  /**
   * Test ElementHandler {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testElementHandlerOnStartElement_givenJavaLangObject() throws SAXParseException {
    // Arrange
    ElementHandler elementHandler = new ElementHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant:current", typeClass);

    Locator2Impl locator = new Locator2Impl();
    locator.setSystemId("ant:current");

    AntXMLContext context = new AntXMLContext(project);
    context.setCurrentTarget(new Target());
    context.setLocator(locator);

    // Act
    elementHandler.onStartElement("ant:current", "Tag", "Qname", attrs, context);

    // Assert
    Task[] tasks = context.getCurrentTarget().getTasks();
    Task task = tasks[0];
    assertTrue(task instanceof UnknownElement);
    assertEquals("", ((UnknownElement) task).getNamespace());
    assertEquals("Tag", task.getTaskType());
    Vector<RuntimeConfigurable> wrapperStack = context.getWrapperStack();
    assertEquals(1, wrapperStack.size());
    assertEquals(1, tasks.length);
    assertTrue(wrapperStack.get(0).getAttributeMap().isEmpty());
  }

  /**
   * Test ElementHandler {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>Then first element Location FileName is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testElementHandlerOnStartElement_thenFirstElementLocationFileNameIsEmptyString()
      throws SAXParseException {
    // Arrange
    ElementHandler elementHandler = new ElementHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    Locator2Impl locator = new Locator2Impl();
    locator.setSystemId("file:");

    AntXMLContext context = new AntXMLContext(new Project());
    context.setCurrentTarget(new Target());
    context.setLocator(locator);

    // Act
    elementHandler.onStartElement("Uri", "Tag", "Qname", attrs, context);

    // Assert
    Task[] tasks = context.getCurrentTarget().getTasks();
    Task task = tasks[0];
    assertTrue(task instanceof UnknownElement);
    assertEquals("", task.getLocation().getFileName());
    assertEquals("Uri", ((UnknownElement) task).getNamespace());
    assertEquals("Uri:Tag", task.getTaskType());
    assertEquals(1, context.getWrapperStack().size());
    assertEquals(1, tasks.length);
  }

  /**
   * Test ElementHandler {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>Then first element Namespace is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testElementHandlerOnStartElement_thenFirstElementNamespaceIsEmptyString() throws SAXParseException {
    // Arrange
    ElementHandler elementHandler = new ElementHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    Locator2Impl locator = new Locator2Impl();
    locator.setSystemId("ant:current");

    AntXMLContext context = new AntXMLContext(new Project());
    context.setCurrentTarget(new Target());
    context.setLocator(locator);

    // Act
    elementHandler.onStartElement("ant:current", "Tag", "Qname", attrs, context);

    // Assert
    Task[] tasks = context.getCurrentTarget().getTasks();
    Task task = tasks[0];
    assertTrue(task instanceof UnknownElement);
    assertEquals("", ((UnknownElement) task).getNamespace());
    assertEquals("Tag", task.getTaskType());
    Vector<RuntimeConfigurable> wrapperStack = context.getWrapperStack();
    assertEquals(1, wrapperStack.size());
    assertEquals(1, tasks.length);
    assertTrue(wrapperStack.get(0).getAttributeMap().isEmpty());
  }

  /**
   * Test ElementHandler {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>When {@code Uri}.</li>
   *   <li>Then first element Namespace is {@code Uri}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ElementHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testElementHandlerOnStartElement_whenUri_thenFirstElementNamespaceIsUri() throws SAXParseException {
    // Arrange
    ElementHandler elementHandler = new ElementHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    Locator2Impl locator = new Locator2Impl();
    locator.setSystemId("ant:current");

    AntXMLContext context = new AntXMLContext(new Project());
    context.setCurrentTarget(new Target());
    context.setLocator(locator);

    // Act
    elementHandler.onStartElement("Uri", "Tag", "Qname", attrs, context);

    // Assert
    Task[] tasks = context.getCurrentTarget().getTasks();
    Task task = tasks[0];
    assertTrue(task instanceof UnknownElement);
    assertEquals("Uri", ((UnknownElement) task).getNamespace());
    assertEquals("Uri:Tag", task.getTaskType());
    Vector<RuntimeConfigurable> wrapperStack = context.getWrapperStack();
    assertEquals(1, wrapperStack.size());
    assertEquals(1, tasks.length);
    assertTrue(wrapperStack.get(0).getAttributeMap().isEmpty());
  }

  /**
   * Test MainHandler {@link MainHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then throw {@link SAXParseException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MainHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testMainHandlerOnStartChild_whenName_thenThrowSAXParseException() throws SAXParseException {
    // Arrange
    MainHandler mainHandler = new MainHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    // Act and Assert
    assertThrows(SAXParseException.class,
        () -> mainHandler.onStartChild("Uri", "Name", "Qname", attrs, new AntXMLContext(new Project())));
  }

  /**
   * Test MainHandler {@link MainHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>When {@code project}.</li>
   *   <li>Then throw {@link SAXParseException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MainHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testMainHandlerOnStartChild_whenProject_thenThrowSAXParseException() throws SAXParseException {
    // Arrange
    MainHandler mainHandler = new MainHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    // Act and Assert
    assertThrows(SAXParseException.class,
        () -> mainHandler.onStartChild("Uri", "project", "Qname", attrs, new AntXMLContext(new Project())));
  }

  /**
   * Test MainHandler {@link MainHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>When {@code Uri}.</li>
   *   <li>Then throw {@link SAXParseException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MainHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testMainHandlerOnStartChild_whenUri_thenThrowSAXParseException() throws SAXParseException {
    // Arrange
    MainHandler mainHandler = new MainHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    // Act and Assert
    assertThrows(SAXParseException.class,
        () -> mainHandler.onStartChild("Uri", "Qname", "Qname", attrs, new AntXMLContext(new Project())));
  }

  /**
   * Test {@link ProjectHelper2#parseAntlibDescriptor(Project, Resource)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parseAntlibDescriptor(Project, Resource)}
   */
  @Test
  public void testParseAntlibDescriptor_whenFileResource_thenThrowBuildException() {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();
    Project containingProject = new Project();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> projectHelper2.parseAntlibDescriptor(containingProject, new FileResource()));
  }

  /**
   * Test {@link ProjectHelper2#parseAntlibDescriptor(Project, Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with name is {@code Unsupported resource type:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parseAntlibDescriptor(Project, Resource)}
   */
  @Test
  public void testParseAntlibDescriptor_whenResourceWithNameIsUnsupportedResourceType() {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();
    Project containingProject = new Project();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> projectHelper2.parseAntlibDescriptor(containingProject, new Resource("Unsupported resource type: ")));
  }

  /**
   * Test {@link ProjectHelper2#parseAntlibDescriptor(Project, Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parseAntlibDescriptor(Project, Resource)}
   */
  @Test
  public void testParseAntlibDescriptor_whenResource_thenThrowBuildException() {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();
    Project containingProject = new Project();

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parseAntlibDescriptor(containingProject, new Resource()));
  }

  /**
   * Test {@link ProjectHelper2#parseUnknownElement(Project, URL)}.
   * <p>
   * Method under test: {@link ProjectHelper2#parseUnknownElement(Project, URL)}
   */
  @Test
  public void testParseUnknownElement() throws MalformedURLException, BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parseUnknownElement(new Project(),
        Paths.get(System.getProperty("java.io.tmpdir"), "!/").toUri().toURL()));
  }

  /**
   * Test {@link ProjectHelper2#parseUnknownElement(Project, URL)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parseUnknownElement(Project, URL)}
   */
  @Test
  public void testParseUnknownElement_givenAntClassLoader() throws MalformedURLException, BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parseUnknownElement(project,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL()));
  }

  /**
   * Test {@link ProjectHelper2#parseUnknownElement(Project, URL)}.
   * <ul>
   *   <li>Given {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parseUnknownElement(Project, URL)}
   */
  @Test
  public void testParseUnknownElement_givenDefaultLogger() throws MalformedURLException, BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parseUnknownElement(project,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL()));
  }

  /**
   * Test {@link ProjectHelper2#parseUnknownElement(Project, URL)}.
   * <ul>
   *   <li>Given {@code !/}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parseUnknownElement(Project, URL)}
   */
  @Test
  public void testParseUnknownElement_givenExclamationMarkSlash() throws MalformedURLException, BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("!/", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parseUnknownElement(project,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL()));
  }

  /**
   * Test {@link ProjectHelper2#parseUnknownElement(Project, URL)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parseUnknownElement(Project, URL)}
   */
  @Test
  public void testParseUnknownElement_whenProject_thenThrowBuildException()
      throws MalformedURLException, BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parseUnknownElement(new Project(),
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL()));
  }

  /**
   * Test {@link ProjectHelper2#parseUnknownElement(Project, URL)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code .} toUri toURL.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parseUnknownElement(Project, URL)}
   */
  @Test
  public void testParseUnknownElement_whenPropertyIsJavaIoTmpdirIsDotToUriToURL()
      throws MalformedURLException, BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parseUnknownElement(new Project(),
        Paths.get(System.getProperty("java.io.tmpdir"), ".").toUri().toURL()));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object, RootHandler)} with {@code project}, {@code source}, {@code handler}.
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object, RootHandler)}
   */
  @Test
  public void testParseWithProjectSourceHandler() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();
    Project project = new Project();
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    AntXMLContext context = new AntXMLContext(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parse(project, fileResource,
        new RootHandler(context, ProjectHelper2.getElementHandler())));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object, RootHandler)} with {@code project}, {@code source}, {@code handler}.
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object, RootHandler)}
   */
  @Test
  public void testParseWithProjectSourceHandler2() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();
    Project project = new Project();
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        "Source ");

    AntXMLContext context = new AntXMLContext(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parse(project, fileResource,
        new RootHandler(context, ProjectHelper2.getElementHandler())));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object, RootHandler)} with {@code project}, {@code source}, {@code handler}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object, RootHandler)}
   */
  @Test
  public void testParseWithProjectSourceHandler_givenAntClassLoader() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());
    File toFileResult = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    AntXMLContext context = new AntXMLContext(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parse(project, toFileResult,
        new RootHandler(context, ProjectHelper2.getElementHandler())));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object, RootHandler)} with {@code project}, {@code source}, {@code handler}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object, RootHandler)}
   */
  @Test
  public void testParseWithProjectSourceHandler_givenAntClassLoader2() throws MalformedURLException, BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());
    URL toURLResult = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL();
    AntXMLContext context = new AntXMLContext(new Project());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> projectHelper2.parse(project, toURLResult, new RootHandler(context, ProjectHelper2.getElementHandler())));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object, RootHandler)} with {@code project}, {@code source}, {@code handler}.
   * <ul>
   *   <li>Given {@code var}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object, RootHandler)}
   */
  @Test
  public void testParseWithProjectSourceHandler_givenVar() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("var", typeClass);
    File toFileResult = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    AntXMLContext context = new AntXMLContext(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parse(project, toFileResult,
        new RootHandler(context, ProjectHelper2.getElementHandler())));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object, RootHandler)} with {@code project}, {@code source}, {@code handler}.
   * <ul>
   *   <li>When {@link FileResource#FileResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object, RootHandler)}
   */
  @Test
  public void testParseWithProjectSourceHandler_whenFileResource_thenThrowBuildException() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();
    Project project = new Project();
    FileResource fileResource = new FileResource();
    AntXMLContext context = new AntXMLContext(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parse(project, fileResource,
        new RootHandler(context, ProjectHelper2.getElementHandler())));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object, RootHandler)} with {@code project}, {@code source}, {@code handler}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code ..} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object, RootHandler)}
   */
  @Test
  public void testParseWithProjectSourceHandler_whenPropertyIsJavaIoTmpdirIsDotDotToFile() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();
    Project project = new Project();
    File toFileResult = Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile();
    AntXMLContext context = new AntXMLContext(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parse(project, toFileResult,
        new RootHandler(context, ProjectHelper2.getElementHandler())));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object, RootHandler)} with {@code project}, {@code source}, {@code handler}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code .} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object, RootHandler)}
   */
  @Test
  public void testParseWithProjectSourceHandler_whenPropertyIsJavaIoTmpdirIsDotToFile() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();
    Project project = new Project();
    File toFileResult = Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile();
    AntXMLContext context = new AntXMLContext(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parse(project, toFileResult,
        new RootHandler(context, ProjectHelper2.getElementHandler())));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object, RootHandler)} with {@code project}, {@code source}, {@code handler}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code file:} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object, RootHandler)}
   */
  @Test
  public void testParseWithProjectSourceHandler_whenPropertyIsJavaIoTmpdirIsFileToFile() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();
    Project project = new Project();
    File toFileResult = Paths.get(System.getProperty("java.io.tmpdir"), "file:").toFile();
    AntXMLContext context = new AntXMLContext(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parse(project, toFileResult,
        new RootHandler(context, ProjectHelper2.getElementHandler())));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object, RootHandler)} with {@code project}, {@code source}, {@code handler}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object, RootHandler)}
   */
  @Test
  public void testParseWithProjectSourceHandler_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();
    Project project = new Project();
    File toFileResult = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    AntXMLContext context = new AntXMLContext(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parse(project, toFileResult,
        new RootHandler(context, ProjectHelper2.getElementHandler())));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object, RootHandler)} with {@code project}, {@code source}, {@code handler}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toUri toURL.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object, RootHandler)}
   */
  @Test
  public void testParseWithProjectSourceHandler_whenPropertyIsJavaIoTmpdirIsTestTxtToUriToURL()
      throws MalformedURLException, BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();
    Project project = new Project();
    URL toURLResult = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL();
    AntXMLContext context = new AntXMLContext(new Project());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> projectHelper2.parse(project, toURLResult, new RootHandler(context, ProjectHelper2.getElementHandler())));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object, RootHandler)} with {@code project}, {@code source}, {@code handler}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object, RootHandler)}
   */
  @Test
  public void testParseWithProjectSourceHandler_whenResource_thenThrowBuildException() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();
    Project project = new Project();
    Resource resource = new Resource();
    AntXMLContext context = new AntXMLContext(new Project());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> projectHelper2.parse(project, resource, new RootHandler(context, ProjectHelper2.getElementHandler())));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object, RootHandler)} with {@code project}, {@code source}, {@code handler}.
   * <ul>
   *   <li>When {@code Source}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object, RootHandler)}
   */
  @Test
  public void testParseWithProjectSourceHandler_whenSource_thenThrowBuildException() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();
    Project project = new Project();
    AntXMLContext context = new AntXMLContext(new Project());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> projectHelper2.parse(project, "Source", new RootHandler(context, ProjectHelper2.getElementHandler())));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object)} with {@code project}, {@code source}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object)}
   */
  @Test
  public void testParseWithProjectSource_givenAntClassLoader() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parse(project, "Source"));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object)} with {@code project}, {@code source}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object)}
   */
  @Test
  public void testParseWithProjectSource_givenJavaLangObject() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parse(project, "Source"));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object)} with {@code project}, {@code source}.
   * <ul>
   *   <li>Given {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object)}
   */
  @Test
  public void testParseWithProjectSource_givenValue() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parse(project, "Source"));
  }

  /**
   * Test {@link ProjectHelper2#parse(Project, Object)} with {@code project}, {@code source}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper2#parse(Project, Object)}
   */
  @Test
  public void testParseWithProjectSource_whenProject_thenThrowBuildException() throws BuildException {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper2.parse(new Project(), "Source"));
  }

  /**
   * Test ProjectHandler {@link ProjectHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testProjectHandlerOnStartChild_whenName() throws SAXParseException {
    // Arrange
    ProjectHandler projectHandler = new ProjectHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    // Act
    AntHandler actualOnStartChildResult = projectHandler.onStartChild("Uri", "Name", "Qname", attrs,
        new AntXMLContext(new Project()));

    // Assert
    assertTrue(actualOnStartChildResult instanceof ElementHandler);
    assertSame(actualOnStartChildResult, actualOnStartChildResult.onStartChild("Uri", "Tag", "Qname", null, null));
  }

  /**
   * Test ProjectHandler {@link ProjectHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>When {@code Uri}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testProjectHandlerOnStartChild_whenUri() throws SAXParseException {
    // Arrange
    ProjectHandler projectHandler = new ProjectHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    // Act
    AntHandler actualOnStartChildResult = projectHandler.onStartChild("Uri", "target", "Qname", attrs,
        new AntXMLContext(new Project()));

    // Assert
    assertTrue(actualOnStartChildResult instanceof ElementHandler);
    assertSame(actualOnStartChildResult, actualOnStartChildResult.onStartChild("Uri", "Tag", "Qname", null, null));
  }

  /**
   * Test ProjectHandler {@link ProjectHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>Given {@code Uri}.</li>
   *   <li>Then throw {@link SAXParseException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testProjectHandlerOnStartElement_givenUri_thenThrowSAXParseException() throws SAXParseException {
    // Arrange
    ProjectHandler projectHandler = new ProjectHandler();

    Attributes2Impl attrs = new Attributes2Impl();
    attrs.addAttribute("Uri", "Uri", "Uri", "Uri", "Uri");
    attrs.addAttribute("ant.file.", "ant.file.", "ant.file.", "ant.file.", "ant.file.");

    Locator2Impl locator = new Locator2Impl();
    locator.setSystemId("ant.file.");

    AntXMLContext context = new AntXMLContext(new Project());
    context.setLocator(locator);

    // Act and Assert
    assertThrows(SAXParseException.class, () -> projectHandler.onStartElement("Uri", "Tag", "Qname", attrs, context));
  }

  /**
   * Test RootHandler {@link RootHandler#getCurrentAntHandler()}.
   * <p>
   * Method under test: {@link RootHandler#getCurrentAntHandler()}
   */
  @Test
  public void testRootHandlerGetCurrentAntHandler() {
    // Arrange
    AntXMLContext context = new AntXMLContext(new Project());
    AntHandler rootHandler = ProjectHelper2.getElementHandler();

    // Act
    AntHandler actualCurrentAntHandler = (new RootHandler(context, rootHandler)).getCurrentAntHandler();

    // Assert
    assertTrue(actualCurrentAntHandler instanceof ElementHandler);
    assertSame(rootHandler, actualCurrentAntHandler);
  }

  /**
   * Test RootHandler {@link RootHandler#RootHandler(AntXMLContext, AntHandler)}.
   * <p>
   * Method under test: {@link RootHandler#RootHandler(AntXMLContext, AntHandler)}
   */
  @Test
  public void testRootHandlerNewRootHandler() {
    // Arrange
    AntXMLContext context = new AntXMLContext(new Project());
    AntHandler rootHandler = ProjectHelper2.getElementHandler();

    // Act and Assert
    AntHandler currentAntHandler = (new RootHandler(context, rootHandler)).getCurrentAntHandler();
    assertTrue(currentAntHandler instanceof ElementHandler);
    assertSame(rootHandler, currentAntHandler);
  }

  /**
   * Test RootHandler {@link RootHandler#resolveEntity(String, String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RootHandler#resolveEntity(String, String)}
   */
  @Test
  public void testRootHandlerResolveEntity_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());
    AntXMLContext context = new AntXMLContext(project);

    // Act and Assert
    assertNull((new RootHandler(context, ProjectHelper2.getElementHandler())).resolveEntity("42", "42"));
  }

  /**
   * Test RootHandler {@link RootHandler#resolveEntity(String, String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link RootHandler#resolveEntity(String, String)}
   */
  @Test
  public void testRootHandlerResolveEntity_givenProjectAddBuildListenerDefaultLogger() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());
    AntXMLContext context = new AntXMLContext(project);

    // Act and Assert
    assertNull((new RootHandler(context, ProjectHelper2.getElementHandler())).resolveEntity("42", "42"));
  }

  /**
   * Test RootHandler {@link RootHandler#resolveEntity(String, String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link ExecutorTest} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link RootHandler#resolveEntity(String, String)}
   */
  @Test
  public void testRootHandlerResolveEntity_givenProjectAddBuildListenerExecutorTest() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new ExecutorTest());
    AntXMLContext context = new AntXMLContext(project);

    // Act and Assert
    assertNull((new RootHandler(context, ProjectHelper2.getElementHandler())).resolveEntity("42", "42"));
  }

  /**
   * Test RootHandler {@link RootHandler#resolveEntity(String, String)}.
   * <ul>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RootHandler#resolveEntity(String, String)}
   */
  @Test
  public void testRootHandlerResolveEntity_thenReturnNull() {
    // Arrange
    AntXMLContext context = new AntXMLContext(new Project());

    // Act and Assert
    assertNull((new RootHandler(context, ProjectHelper2.getElementHandler())).resolveEntity("42", "42"));
  }

  /**
   * Test TargetHandler {@link TargetHandler#onEndElement(String, String, AntXMLContext)}.
   * <p>
   * Method under test: {@link TargetHandler#onEndElement(String, String, AntXMLContext)}
   */
  @Test
  public void testTargetHandlerOnEndElement() {
    // Arrange
    TargetHandler targetHandler = new TargetHandler();
    Project project = new Project();
    AntXMLContext context = new AntXMLContext(project);

    // Act
    targetHandler.onEndElement("Uri", "Tag", context);

    // Assert
    Target currentTarget = context.getCurrentTarget();
    assertEquals("", currentTarget.getName());
    assertEquals("", currentTarget.toString());
    assertNull(currentTarget.getDescription());
    assertNull(currentTarget.getIf());
    assertNull(currentTarget.getUnless());
    assertEquals(0, currentTarget.getTasks().length);
    Vector<Target> targets = context.getTargets();
    assertEquals(1, targets.size());
    assertSame(project, currentTarget.getProject());
    assertSame(currentTarget, targets.get(0));
    assertSame(currentTarget, context.getImplicitTarget());
  }

  /**
   * Test TargetHandler {@link TargetHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}.
   * <p>
   * Method under test: {@link TargetHandler#onStartChild(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testTargetHandlerOnStartChild() throws SAXParseException {
    // Arrange
    TargetHandler targetHandler = new TargetHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    // Act
    AntHandler actualOnStartChildResult = targetHandler.onStartChild("Uri", "Name", "Qname", attrs,
        new AntXMLContext(new Project()));

    // Assert
    assertTrue(actualOnStartChildResult instanceof ElementHandler);
    assertSame(actualOnStartChildResult, actualOnStartChildResult.onStartChild("Uri", "Tag", "Qname", null, null));
  }

  /**
   * Test TargetHandler {@link TargetHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>Given empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TargetHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testTargetHandlerOnStartElement_givenEmptyString() throws SAXParseException {
    // Arrange
    TargetHandler targetHandler = new TargetHandler();

    Attributes2Impl attrs = new Attributes2Impl();
    attrs.addAttribute("", "Tag", "Tag", "Tag", "Tag");

    AntXMLContext context = new AntXMLContext(new Project());
    context.setLocator(new Locator2Impl());

    // Act and Assert
    assertThrows(SAXParseException.class, () -> targetHandler.onStartElement("Uri", "Tag", "Qname", attrs, context));
  }

  /**
   * Test TargetHandler {@link TargetHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>Given {@link Locator2Impl#Locator2Impl()}.</li>
   *   <li>Then throw {@link SAXParseException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TargetHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testTargetHandlerOnStartElement_givenLocator2Impl_thenThrowSAXParseException() throws SAXParseException {
    // Arrange
    TargetHandler targetHandler = new TargetHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    AntXMLContext context = new AntXMLContext(new Project());
    context.setLocator(new Locator2Impl());

    // Act and Assert
    assertThrows(SAXParseException.class, () -> targetHandler.onStartElement("Uri", "Tag", "Qname", attrs, context));
  }

  /**
   * Test TargetHandler {@link TargetHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>When {@link Attributes2Impl#addAttribute(String, String, String, String, String)} with {@code null} and {@code Tag} and {@code Tag} and {@code Tag} and {@code Tag}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TargetHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testTargetHandlerOnStartElement_whenAddAttributeWithNullAndTagAndTagAndTagAndTag()
      throws SAXParseException {
    // Arrange
    TargetHandler targetHandler = new TargetHandler();

    Attributes2Impl attrs = new Attributes2Impl();
    attrs.addAttribute(null, "Tag", "Tag", "Tag", "Tag");

    AntXMLContext context = new AntXMLContext(new Project());
    context.setLocator(new Locator2Impl());

    // Act and Assert
    assertThrows(SAXParseException.class, () -> targetHandler.onStartElement("Uri", "Tag", "Qname", attrs, context));
  }

  /**
   * Test TargetHandler {@link TargetHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>When {@link Attributes2Impl#addAttribute(String, String, String, String, String)} with {@code Tag} and {@code Tag} and {@code Tag} and {@code Tag} and {@code Tag}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TargetHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testTargetHandlerOnStartElement_whenAddAttributeWithTagAndTagAndTagAndTagAndTag()
      throws SAXParseException {
    // Arrange
    TargetHandler targetHandler = new TargetHandler();

    Attributes2Impl attrs = new Attributes2Impl();
    attrs.addAttribute("Tag", "Tag", "Tag", "Tag", "Tag");

    AntXMLContext context = new AntXMLContext(new Project());
    context.setLocator(new Locator2Impl());

    // Act and Assert
    assertThrows(SAXParseException.class, () -> targetHandler.onStartElement("Uri", "Tag", "Qname", attrs, context));
  }

  /**
   * Test TargetHandler {@link TargetHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>When {@link Attributes2Impl#addAttribute(String, String, String, String, String)} with {@code Uri} and {@code Uri} and {@code Uri} and {@code Uri} and {@code Uri}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TargetHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testTargetHandlerOnStartElement_whenAddAttributeWithUriAndUriAndUriAndUriAndUri()
      throws SAXParseException {
    // Arrange
    TargetHandler targetHandler = new TargetHandler();

    Attributes2Impl attrs = new Attributes2Impl();
    attrs.addAttribute("Uri", "Uri", "Uri", "Uri", "Uri");
    attrs.addAttribute("Tag", "Tag", "Tag", "Tag", "Tag");

    AntXMLContext context = new AntXMLContext(new Project());
    context.setLocator(new Locator2Impl());

    // Act and Assert
    assertThrows(SAXParseException.class, () -> targetHandler.onStartElement("Uri", "Tag", "Qname", attrs, context));
  }

  /**
   * Test TargetHandler {@link TargetHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}.
   * <ul>
   *   <li>When {@code target}.</li>
   *   <li>Then throw {@link SAXParseException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TargetHandler#onStartElement(String, String, String, Attributes, AntXMLContext)}
   */
  @Test
  public void testTargetHandlerOnStartElement_whenTarget_thenThrowSAXParseException() throws SAXParseException {
    // Arrange
    TargetHandler targetHandler = new TargetHandler();
    Attributes2Impl attrs = new Attributes2Impl();

    AntXMLContext context = new AntXMLContext(new Project());
    context.setLocator(new Locator2Impl());

    // Act and Assert
    assertThrows(SAXParseException.class, () -> targetHandler.onStartElement("Uri", "target", "Qname", attrs, context));
  }
}
