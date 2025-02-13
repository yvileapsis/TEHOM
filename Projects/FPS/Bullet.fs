namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

type BulletCommand =
    | Update
    | Penetration
    interface Command

type BulletDispatcher () =
    inherit Entity3dDispatcher<int64, Message, BulletCommand> (true, false, false, fun world -> world.UpdateTime)

    static let [<Literal>] BulletLifeTime = 256L

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelFacet>]

    override this.Definitions (_, _) =
        [Entity.Size == v3 1f 1f 1f
         Entity.Scale == v3 0.1f 0.1f 0.1f
         Entity.Presence == Omnipresent
         Entity.Static == false
         Entity.BodyType == Dynamic
         Entity.BodyShape == SphereShape { Radius = 0.5f; TransformOpt = None; PropertiesOpt = None }
         Entity.Restitution == 0.5f
         Entity.LinearDamping == 0.0f
         Entity.Substance == Density 0.1f
         Entity.GravityOverride == Some v3Zero
         Entity.Observable == true
         Entity.StaticModel == Assets.Default.BallModel
         Entity.UpdateEvent => Update
         Entity.BodyPenetrationEvent => Penetration]

    override this.Command (startTime, command, entity, world) =
        match command with
        | Update ->
            let localTime = world.UpdateTime - startTime
            let world = if localTime = BulletLifeTime then World.destroyEntity entity world else world
            just world
        | Penetration ->
            //let world = World.destroyEntity entity world
            just world